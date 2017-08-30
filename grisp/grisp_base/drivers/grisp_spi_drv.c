/* spi_drv.c */

#include <assert.h>
#include <fcntl.h>
#include <rtems.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/ioctl.h>

#include <bsp.h>
#include <bsp/atsam-spi.h>
#include <bsp/spi.h>
#include <dev/spi/spi.h>

#include "erl_driver.h"
#include "sys.h"


int grisp_spi_init (void);
ErlDrvData grisp_spi_start (ErlDrvPort port, char *command);
void grisp_spi_stop (ErlDrvData drv_data);
void grisp_spi_output (ErlDrvData drv_data, char *buf, ErlDrvSizeT len);

ErlDrvEntry grisp_spi_driver_entry = {
    grisp_spi_init,
    grisp_spi_start,
    grisp_spi_stop,
    grisp_spi_output,
    NULL,
    NULL,
    "grisp_spi_drv",
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

struct grisp_spi_data {
    ErlDrvPort port;
    char cnt;
    int fd;
};

/* FIXME: consider making this more dynamic, especially if generalized */
static struct grisp_spi_data grisp_spi_data = { NULL, 0, -1 };

/* Make sure to keep this at sync with the -define(res_max_size.. in spi.erl */
#define RES_MAX_SIZE 256

int grisp_spi_init (void)
{
    int rv;

    /* bus registration */
    rv = spi_bus_register_atsam(ATSAM_SPI_0_BUS_PATH, ID_SPI0, SPI0, NULL, 0);
    assert(rv == 0);
    return 0;
}

ErlDrvData grisp_spi_start (ErlDrvPort port, char *command)
{
    int rv;
    uint32_t speed = 100000;

    if (grisp_spi_data.port != NULL)
        return ERL_DRV_ERROR_GENERAL;

    grisp_spi_data.port = port;
    grisp_spi_data.cnt = 1;

    grisp_spi_data.fd = open(ATSAM_SPI_0_BUS_PATH, O_RDWR);
    assert(grisp_spi_data.fd != -1);

    rv = ioctl(grisp_spi_data.fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed);
    assert(rv == 0);

    return (ErlDrvData)&grisp_spi_data;
}

void grisp_spi_stop (ErlDrvData drv_data)
{
    ASSERT ((struct grisp_spi_data *)drv_data == &grisp_spi_data);
    close(grisp_spi_data.fd);
    grisp_spi_data.port = NULL;
}

static void
test_set_default_msg(struct spi_ioc_transfer *msg)
{
    msg->rx_buf = NULL;
    msg->tx_buf = NULL;
    msg->len = 0;
    msg->speed_hz = 100000;
    msg->delay_usecs = 1;
    msg->bits_per_word = 8;
    msg->cs_change = 1;
    msg->rx_nbits = 0;
    msg->tx_nbits = 0;
    msg->mode = 0;
    msg->cs = 2;
}

void grisp_spi_output (ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    int rv;
    uint cs;
    char res[RES_MAX_SIZE];
    struct spi_ioc_transfer msg;

    // Grab first byte as chip select, and shorten buffer by 1
    cs = buf[0];
    buf++;
    len -= 1;

    ASSERT ((struct grisp_spi_data *)drv_data == &grisp_spi_data);
    ASSERT (grisp_spi_data.port != NULL);

    grisp_spi_data.cnt++;

    assert(len <= RES_MAX_SIZE);
    test_set_default_msg(&msg);
    msg.cs = cs;
    msg.tx_buf = buf;
    msg.rx_buf = res;
    msg.len = len;

    rv = ioctl(grisp_spi_data.fd, SPI_IOC_MESSAGE(1), &msg);
    assert(rv == 0);

    driver_output(grisp_spi_data.port, res, len);
}
