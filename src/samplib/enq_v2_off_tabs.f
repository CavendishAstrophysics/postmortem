C+ENQ_V2_OFF_TABS

       subroutine enq_v2_off_tabs ( off_type, off_aes,
     :                              off_angle, off_time,
     :                              ha_5pt, dec_5pt,
     :                              off2Nx, off2Ny, 
     :                              offzero, offrow, s)
C
C     Returns tables of pointing offsets
C     added offrow to parameter list: 13 Jan 99 GP
C     Returned:
C         Offset observation type
              integer     off_type
C         Aerial offset control
              integer     off_aes(*)
C         Offset angle (arcmin on sky)
              real        off_angle
C         Offset time in samples per point
              integer     off_time
C         starting row for raster offsets 
              integer     offrow
C         Tables of pointing offsets
              integer     ha_5pt(0:4)
              integer     dec_5pt(0:4)
C         Counts for raster offset (maximum offset from centre)
              integer     off2Nx, off2Ny
C         Counts at centre after each raster row
              integer     offzero
C         Status
              integer     s
C
C     Control tables version 2 support routine for ENQ_OFF_TABS.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'

      integer  i

      if ( s .ne. 0 ) return

      off_type  = offset
      if (offset_arcmin.ne.0.0) then
        off_angle = offset_arcmin
      else
        off_angle = offset_angle
      endif
      off_time  = offset_time

      do i = 1, max_aes
        off_aes(i) = offset_aerial(i)
      enddo

      do i = 0, 4
        ha_5pt(i)  = ha_5point(i)
        dec_5pt(i) = dec_5point(i)
      enddo

      off2Nx  = offset2Nx
      off2Ny  = offset2Ny
      offzero = offset_zero
      offrow  = offset_row

      end



