/* grisp_ir_drv.c */

#include <assert.h>
#include <fcntl.h>
#include <rtems.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/ioctl.h>

#include <bsp.h>
#include <bsp/pin-config.h>

#include "erl_driver.h"
#include "sys.h"

#define N_PINS (sizeof(pins)/sizeof(pins[0]))
#define GRISP_REGISTER_IR 1
#define GRISP_DISABLE_IR 2
static Pin pins[] =
  {
    {PIO_PC12, PIOC, ID_PIOC, PIO_INPUT, PIO_DEFAULT},    /* 0: gpio1 pin1 */
    {PIO_PC13, PIOC, ID_PIOC, PIO_INPUT, PIO_DEFAULT},    /* 1: gpio1 pin2 */
    {PIO_PA21, PIOA, ID_PIOA, PIO_INPUT, PIO_PULLUP},    /* 2: gpio1 pin3 */
    {PIO_PD30, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},    /* 3: gpio1 pin4 */
    {PIO_PD0, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 4: gpio2 pin1 */
    {PIO_PD1, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 5: gpio2 pin2 */
    {PIO_PD2, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 6: gpio2 pin3 */
    {PIO_PD3, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 7: gpio2 pin4 */
    {PIO_PA24, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 8:  led1 r */
    {PIO_PA17, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 9:  led1 g */
    {PIO_PA23, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 10: led1 b */
    {PIO_PA13, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 11: led2 r */
    {PIO_PA5 , PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 12  led2 g */
    {PIO_PA11, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 13: led2 b */
    {PIO_PA6 , PIOA, ID_PIOA, PIO_INPUT, PIO_PULLUP},     /* 14: jumper 1 */
    {PIO_PD9 , PIOD, ID_PIOD, PIO_INPUT, PIO_PULLUP},     /* 15: jumper 2 */
    {PIO_PC17, PIOC, ID_PIOC, PIO_INPUT, PIO_PULLUP},     /* 16: jumper 3 */
    {PIO_PD11, PIOD, ID_PIOD, PIO_INPUT, PIO_PULLUP},     /* 17: jumper 4 */
    {PIO_PC10, PIOC, ID_PIOC, PIO_INPUT, PIO_PULLUP},     /* 18: jumper 5 */
    {PIO_PA9, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},     /* 19: spi1 pin 9 */
    {PIO_PA10, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},    /* 20: spi1 pin10 */
    {PIO_PD12C_SPI0_NPCS2, PIOD, ID_PIOD,
     PIO_PERIPH_C, PIO_DEFAULT},                          /* 21: SS1 */
    {PIO_PD27B_SPI0_NPCS3, PIOD, ID_PIOD,
     PIO_PERIPH_B, PIO_DEFAULT}                           /* 22: SS2 */

  };

struct grisp_ir_data {
  ErlDrvPort port;
  int fd[2];
  rtems_id tid[N_PINS];
  char buf;
};

struct grisp_rtems_event_arg {
  struct grisp_ir_data *data;
  uint8_t index;
};

ErlDrvData grisp_ir_start(ErlDrvPort port, char* command);
void grisp_ir_stop(ErlDrvData data);
void grisp_ir_ready_input(ErlDrvData data, ErlDrvEvent event);
void grisp_ir_stop_select(ErlDrvEvent event, void* reserved);
rtems_task grisp_ir_pin_trigger(rtems_task_argument arg);
void grisp_ir_handle(const Pin *pPIn, void *arg);
void grisp_ir_output(ErlDrvData data, char *buf, ErlDrvSizeT len);

ErlDrvEntry grisp_ir_driver_entry = {
    NULL,
    grisp_ir_start,
    grisp_ir_stop,
    grisp_ir_output,
    grisp_ir_ready_input,
    NULL,
    "grisp_ir_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    grisp_ir_stop_select
};

const Pin *triggerpin = pins+2;

ErlDrvData grisp_ir_start(ErlDrvPort port, char *command)
{
    struct grisp_ir_data *data = (struct grisp_ir_data *)driver_alloc(sizeof(struct grisp_ir_data));

    if(pipe(data->fd) != 0)
        return ERL_DRV_ERROR_GENERAL;
    data->port = port;
    driver_select(data->port, (ErlDrvEvent)data->fd[0], ERL_DRV_READ, 1);
    return (ErlDrvData) data;
}

void grisp_ir_output(ErlDrvData arg, char *buf, ErlDrvSizeT len){
  struct grisp_ir_data *data = (struct grisp_ir_data *) arg;

  uint8_t res, index, cmd, type, attribute;
  rtems_name taskname;
  struct grisp_rtems_event_arg *earg;
  
  index = *buf++;
  cmd = *buf++;
  
  if (index >= N_PINS) {
    driver_failure_atom(data->port, "pin_index_range");
    return;
  }
  if (pins[index].pio == PIOC){
    driver_failure_atom(data->port, "no_interrupt_allowed");
    return;
  }

  switch(cmd){
  case GRISP_REGISTER_IR:
    type = *buf++;
    attribute = *buf++;
    
    /* Setup Pins for Interrupt */
    pins[index].type = type;
    pins[index].attribute = attribute;

    earg = (struct grisp_rtems_event_arg *)driver_alloc(sizeof(struct grisp_rtems_event_arg));
    earg->data = data;
    earg->index = index;
    /* Start task to receive interrupts */
    taskname = rtems_build_name('G','I','R',(char) index);
    assert(rtems_task_create(taskname, 8, 1024, RTEMS_DEFAULT_MODES, RTEMS_DEFAULT_ATTRIBUTES, &(data->tid[index])) == RTEMS_SUCCESSFUL);
    assert(rtems_task_start(data->tid[index], &grisp_ir_pin_trigger, (rtems_task_argument) earg) == RTEMS_SUCCESSFUL);
    PIO_ConfigureIt(pins+index, &grisp_ir_handle, (void *) &(data->tid[index]));
    PIO_EnableIt(pins+index);
    driver_output(data->port, &res, 0);
    break;
  case GRISP_DISABLE_IR:
    PIO_DisableIt(pins+index);
    rtems_event_send(data->tid[index], RTEMS_EVENT_6);
    driver_output(data->port, &res, 0);
    break;
  default:
    driver_failure_atom(data->port, "unknown_command");
    return;
  }
}

/* RTEMS Task to receive event and write to pipe */
rtems_task grisp_ir_pin_trigger(rtems_task_argument arg){
  struct grisp_rtems_event_arg *earg = (struct grisp_rtems_event_arg *) arg;
  rtems_event_set eset;
  rtems_id tid = earg->data->tid[earg->index];
  
  while(rtems_event_receive(RTEMS_ALL_EVENTS, RTEMS_EVENT_ANY| RTEMS_WAIT, RTEMS_NO_TIMEOUT, &eset) == RTEMS_SUCCESSFUL){
    if (eset == RTEMS_EVENT_5){
      write(earg->data->fd[1], (void *) &(earg->index), sizeof(char));
    }else if(eset == RTEMS_EVENT_6){
      driver_free(earg);
      rtems_task_delete(tid);
      return;
    }
  }
}

/* ISR */
void grisp_ir_handle(const Pin *pPIn, void *arg){
    rtems_id *tid = (rtems_id *) arg;
    rtems_event_send(*tid, RTEMS_EVENT_5);
}

/* Erlang select callback */
void grisp_ir_ready_input(ErlDrvData arg, ErlDrvEvent event){
    struct grisp_ir_data *data = (struct grisp_ir_data *) arg;
    ssize_t len;

    len = read(data->fd[0], (void *) &(data->buf), sizeof(char));
    if (len<0)
      driver_failure_posix(data->port, errno);
    else
      driver_output(data->port, (char *) &(data->buf), len);
}

void grisp_ir_stop(ErlDrvData arg){
    struct grisp_ir_data *data = (struct grisp_ir_data *) arg;
    PIO_DisableIt(triggerpin);
    driver_select(data->port, (ErlDrvEvent)data->fd[0], ERL_DRV_USE, 0);
    close(data->fd[0]);
    close(data->fd[1]);
    driver_free(data);
}

void grisp_ir_stop_select(ErlDrvEvent event, void* reserved){
    close((int)event);
}
