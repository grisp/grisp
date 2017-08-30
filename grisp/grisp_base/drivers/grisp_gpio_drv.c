/* gpio_drv.c */

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

#define CMD_PIN_CONFIG 1
#define CMD_PIN_GET 2
#define CMD_PIN_CLEAR 3
#define CMD_PIN_SET 4

static Pin pins[] =
  {
    {PIO_PC12, PIOC, ID_PIOC, PIO_INPUT, PIO_DEFAULT},    /* 0: gpio1 pin1 */
    {PIO_PC13, PIOC, ID_PIOC, PIO_INPUT, PIO_DEFAULT},    /* 1: gpio1 pin2 */
    {PIO_PA21, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},    /* 2: gpio1 pin3 */
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

int grisp_gpio_init (void);
ErlDrvData grisp_gpio_start (ErlDrvPort port, char *command);
void grisp_gpio_stop (ErlDrvData drv_data);
void grisp_gpio_output (ErlDrvData drv_data, char *buf, ErlDrvSizeT len);

ErlDrvEntry grisp_gpio_driver_entry = {
    grisp_gpio_init,
    grisp_gpio_start,
    grisp_gpio_stop,
    grisp_gpio_output,
    NULL,
    NULL,
    "grisp_gpio_drv",
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
    NULL
};

struct grisp_gpio_data {
    ErlDrvPort port;
};

static struct grisp_gpio_data grisp_gpio_data = { NULL };

int grisp_gpio_init (void)
{
    return 0;
}

ErlDrvData grisp_gpio_start (ErlDrvPort port, char *command)
{
    int rv;

    if (grisp_gpio_data.port != NULL)
        return ERL_DRV_ERROR_GENERAL;

    grisp_gpio_data.port = port;

    return (ErlDrvData)&grisp_gpio_data;
}

void grisp_gpio_stop (ErlDrvData drv_data)
{
    ASSERT ((struct grisp_gpio_data *)drv_data == &grisp_gpio_data);
    grisp_gpio_data.port = NULL;
}

void grisp_gpio_output (ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    int rv;
    uint cs;
    uint8_t res, index, cmd, type, attribute;

    ASSERT ((struct grisp_gpio_data *)drv_data == &grisp_gpio_data);
    ASSERT (grisp_gpio_data.port != NULL);

    index = *buf++;
    cmd = *buf++;

    if (index >= N_PINS) {
        driver_failure_atom(grisp_gpio_data.port, "pin_index_range");
        return;
    }

    switch (cmd) {
    case CMD_PIN_CONFIG:
        type = *buf++;
        attribute = *buf++;
        pins[index].type = type;
        pins[index].attribute = attribute;
        PIO_Configure(pins + index, 1);
        driver_output(grisp_gpio_data.port, &res, 0);
        break;
    case CMD_PIN_GET:
        res = PIO_Get(pins + index);
        driver_output(grisp_gpio_data.port, &res, 1);
        break;
    case CMD_PIN_CLEAR:
        PIO_Clear(pins + index);
        driver_output(grisp_gpio_data.port, &res, 0);
        break;
    case CMD_PIN_SET:
        PIO_Set(pins + index);
        driver_output(grisp_gpio_data.port, &res, 0);
        break;
    default:
        driver_failure_atom(grisp_gpio_data.port, "pin_index_range");
        return;
    }
}
