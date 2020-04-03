/* grisp_termios_drv.c */

#include <assert.h>
#include <fcntl.h>
#include <rtems.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>

#include "erl_driver.h"
#include "sys.h"

#define DEBUG_PRINT(FMT, ...) erts_printf(FMT "\r\n", ##__VA_ARGS__)

#define BUF_SIZE 256

struct grisp_tio_data {
  ErlDrvPort port;
  int fd;
  char buf[BUF_SIZE];
};

ErlDrvData grisp_tio_start (ErlDrvPort port, char *command);
void grisp_tio_stop (ErlDrvData drv_data);
void grisp_tio_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
void grisp_tio_ready_in(ErlDrvData drv_data, ErlDrvEvent event);
void grisp_tio_ready_out (ErlDrvData drv_data, ErlDrvEvent event);
void grisp_tio_stop_select (ErlDrvEvent event, void* reserved);
void grisp_tio_dowrite(struct grisp_tio_data *data);

#if 0
#define FLAG_VALUE(F, X) {#X, #F, X}

const struct flag_values [] =
  {
   FLAG_VALUE(cflag, CSIZE),
   FLAG_VALUE(cflag, CS8),
  };

/* example  {"CSIZE", "cflag", 0xf} */

struct flag_value
{
  const char *name;
  field
  int32 value;
}
#endif
  
ErlDrvEntry grisp_termios_driver_entry = {
    NULL,
    grisp_tio_start,
    grisp_tio_stop,
    grisp_tio_output,
    grisp_tio_ready_in,
    grisp_tio_ready_out,
    "grisp_termios_drv",
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
    grisp_tio_stop_select,
};

ErlDrvData grisp_tio_start (ErlDrvPort port, char *command)
{
  struct grisp_tio_data *data;
  struct termios term;
  
  data = (struct grisp_tio_data *)driver_alloc(sizeof(struct grisp_tio_data));

  data->port = port;

  data->fd = open("/dev/ttyUSART0", O_RDWR);
  if (data->fd < 0)
    return ERL_DRV_ERROR_ERRNO;

  if (tcgetattr(data->fd, &term) < 0)
    return ERL_DRV_ERROR_ERRNO;

  cfsetispeed(&term, B9600);
  cfsetospeed(&term, B9600);

  term.c_lflag |= ICANON;
  term.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);
  term.c_iflag |= ICRNL;

  term.c_cflag &= ~CSIZE;
  term.c_cflag |= CS8;
  term.c_cflag &= ~(CSTOPB | PARENB);   /* 8N1 */
    
  
  if (tcsetattr(data->fd, TCSAFLUSH, &term) < 0)
    return ERL_DRV_ERROR_ERRNO;

  driver_select(data->port, (ErlDrvEvent)data->fd,
                ERL_DRV_READ | ERL_DRV_WRITE, 1);

  return (ErlDrvData)data;
}

void grisp_tio_stop (ErlDrvData drv_data)
{
  struct grisp_tio_data *data = (struct grisp_tio_data *)drv_data;
  
  driver_select(data->port, (ErlDrvEvent)data->fd, ERL_DRV_USE, 0);
  driver_free(data);
}

void grisp_tio_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
  struct grisp_tio_data *data = (struct grisp_tio_data *)drv_data;

  DEBUG_PRINT("grisp_tio_out: len=%ld", len);
  driver_enq(data->port, buf, len);

  grisp_tio_dowrite(data);
}

void grisp_tio_ready_out (ErlDrvData drv_data, ErlDrvEvent event)
{
  struct grisp_tio_data *data = (struct grisp_tio_data *)drv_data;

  DEBUG_PRINT("grisp_tio_ready_out");
  grisp_tio_dowrite(data);
}
  

void grisp_tio_dowrite(struct grisp_tio_data *data)
{
  struct iovec *iov;
  int i, vlen;
  ssize_t wlen;
  size_t len;

  iov = (struct iovec *)driver_peekq(data->port, &vlen);

  DEBUG_PRINT("grisp_tio_dowrite: vlen=%ld", vlen);
  
  for (i = 0; i < vlen; i++) {
    len = iov[i].iov_len;
    wlen = write(data->fd, iov[i].iov_base, len);
    DEBUG_PRINT("grisp_tio_dowrite: i=%d, len=ld wlen", i, len, wlen);
    driver_deq(data->port, wlen);
    if (wlen != len) break;
  }
}

void grisp_tio_ready_in (ErlDrvData drv_data, ErlDrvEvent event)
{
  ssize_t len;
  
  struct grisp_tio_data *data = (struct grisp_tio_data *)drv_data;

  len = read(data->fd, data->buf, BUF_SIZE);
  if (len < 0)
    driver_failure_posix(data->port, errno);
  else
    driver_output(data->port, data->buf, len);
}

void grisp_tio_stop_select (ErlDrvEvent event, void* reserved)
{
  close((int)event);
}
