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

#define BUF_SIZE 256

ErlDrvData grisp_tio_start (ErlDrvPort port, char *command);
void grisp_tio_stop (ErlDrvData drv_data);
void grisp_tio_ready_in(ErlDrvData drv_data, ErlDrvEvent event);
void grisp_tio_stop_select (ErlDrvEvent event, void* reserved);

ErlDrvEntry grisp_termios_driver_entry = {
    NULL,
    grisp_tio_start,
    grisp_tio_stop,
    NULL,
    grisp_tio_ready_in,
    NULL,
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

struct grisp_tio_data {
  ErlDrvPort port;
  int fd;
  char buf[BUF_SIZE];
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
  term.c_cflag &= ~(CSTOPB | PARENB);
    
  
  if (tcsetattr(data->fd, TCSAFLUSH, &term) < 0)
    return ERL_DRV_ERROR_ERRNO;

  driver_select(data->port, (ErlDrvEvent)data->fd, ERL_DRV_READ, 1);

  return (ErlDrvData)data;
}

void grisp_tio_stop (ErlDrvData drv_data)
{
  struct grisp_tio_data *data = (struct grisp_tio_data *)drv_data;
  
  driver_select(data->port, (ErlDrvEvent)data->fd, ERL_DRV_USE, 0);
  driver_free(data);
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
