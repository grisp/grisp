-module(pmod_uwb_registers).

-export([default/0]).
-export([update_reg/3]).
-export([get_value/2]).

-spec default() -> map().
default() ->
    #{eui => #{eui => <<16#FFFFFFFF00000000:64>>}, % 0x01
      panadr => #{pan_id => <<16#FFFFFFFF:16>>, short_addr => <<16#FFFFFFFF:16>>}, % 0x03
      sys_cfg => #{aackpend => 0, % 0x04
                   autoack => 0,
                   rxautr => 0,
                   rxwtoe => 0,
                   rxm110k => 0,
                   dis_stxp => 0,
                   phr_mode => 0,
                   fcs_init2f => 0,
                   dis_rsde => 0,
                   dis_phe => 0,
                   dis_drxb => 1,
                   dis_fce => 0,
                   spi_edge => 0,
                   hirq_pol => 1,
                   ffa5 => 0,
                   ffa4 => 0,
                   ffar => 0,
                   ffam => 0,
                   ffaa => 0,
                   ffad => 0,
                   ffab => 0,
                   ffbc => 0,
                   ffen => 0},
      tx_fctrl => #{txboffs => 0, % 0x08
                    pe => 1,
                    txpsr => 1,
                    txprf => 1,
                    tr => 0,
                    txbr => 2,
                    r => 0,
                    tfle => 0,
                    tflen => 12},
      rx_fwto => #{rxfwto => 0}, % 0x0C
      sys_ctrl => #{sfcst => 0, % 0x0D
                    txstrt => 0,
                    txdlys => 0,
                    trxoff => 0,
                    wait4resp => 0,
                    rxenab => 0,
                    rxdlye => 0,
                    hrbpt => 0},
      sys_status => #{irqs => 0, % 0x0F
                      cplock => 0,
                      esyncr => 0,
                      aat => 0,
                      txfrb => 0,
                      txprs => 0,
                      pxphs => 0,
                      txfrs => 0,
                      rxprd => 0,
                      rxsfdd => 0,
                      ldedone => 0,
                      rxphd => 0,
                      rxphe => 0,
                      rxdfr => 0,
                      rxfcg => 0,
                      rxfce => 0,
                      rxrfsl => 0,
                      rxrfto => 0,
                      ldeerr => 0,
                      rxovrr => 0,
                      rxpto => 0,
                      gpioirq => 0,
                      slp2init => 0,
                      rfpll_ll => 0,
                      clkpll_ll => 0,
                      rxsfdto => 0,
                      hpdwarn => 0,
                      txberr => 0,
                      affrej => 0,
                      hsrbp => 0,
                      icrbp => 0,
                      rxrscs => 0,
                      rxprej => 0,
                      txpute => 0},
      rx_finfo => #{rxpacc => 1025, % 0x10
                     rxpsr => 0,
                     rxprfr => 0,
                     rng => 0,
                     rxbr => 0,
                     rxfle => 0,
                     rxflen => 0},
      rx_buffer => #{rx_buffer => <<>>}, % 0x11
      rx_fqual => #{fp_ampl2 => 0, % 0x12
                    std_noise => 0,
                    cir_pwr => 1,
                    pp_ampl3 => 0},
      rx_ttcki => #{rxttcki => 0}, % 0x13
      rx_ttcko => #{rmspdel => 0, % 0x14
                    rxtofs => 0,
                    rcphase => 0},
      rx_time => #{rx_stamp => 0, % 0x15
                   fp_index => 0,
                   fp_ampl1 => 0,
                   rx_rawst => 0},
      tx_time => #{tx_stamp => 0, % 0x17
                   tx_rawst => 0},
      rx_sniff => #{sniff_offt => 0, sniff_ont => 0}, % 0x1D
      agc_ctrl => #{agc_ctrl1 => #{dis_am => 1},
                    agc_tune1 => 16#8870,
                    agc_tune2 => 16#2502A907,
                    agc_tune3 => 16#0035,
                    agc_stat1 => #{edv2 => 0,
                                   edg1 => 1}},
      % DRX_CONF isn't complete yet
      drx_conf => #{drx_pretoc => 0, % 0x27
                    rxpacc_nosat => 0}, 
      % PMSC isn't complete yet
      pmsc => #{pmsc_ctrl0 => #{},  % 0x36
                pmsc_ctrl1 => #{arx2init => 0}}
     }.

-spec update_reg(Regs::map(), Reg::atom(), NewVal::atom()|map()) -> map().
update_reg(Regs, Reg, NewVal) ->
    OldVal = maps:get(Reg, Regs),
    if is_map(OldVal) -> maps:put(Reg, maps:merge(OldVal, NewVal), Regs);
       true -> maps:put(Reg, NewVal, Regs) end.

-spec get_value(Regs::map(), Reg::atom()) -> atom()|map().
get_value(Regs, Reg) ->
    maps:get(Reg, Regs).