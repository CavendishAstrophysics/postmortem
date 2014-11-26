C+ENQ_OFF_TABLES

       subroutine enq_off_tables ( lun, offset_type, offset_aerial,
     :                                  offset_angle, offset_time,
     :                                  ha_5point, dec_5point,
     :                                  offset2Nx, offset2Ny,
     :                                  offset_zero, offset_row, s)
C
C     Returns tables of pointing offsets
C     added offset_row to return values: 13 Jan 99  GP

C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C
C     Returned:
C         Offset observation type
              integer     offset_type
C         Aerial offset control
              integer     offset_aerial(*)
C         Offset angle (arcmin on sky)
              real        offset_angle
C         Offset time in samples per point
              integer     offset_time
C         Tables of pointing offsets
              integer     ha_5point(0:4)
              integer     dec_5point(0:4)
C         Counts for raster offset (maximum offset from centre)
              integer     offset2Nx, offset2Ny
C         Counts at centre after each raster row
              integer     offset_zero
C         start rasters at offset_row
              integer     offset_row
C         Status
              integer     s
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .eq. 2) then
         call enq_v2_off_tabs( offset_type, offset_aerial,
     :                         offset_angle, offset_time,
     :                         ha_5point, dec_5point,
     :                         offset2Nx, offset2Ny,
     :                         offset_zero, offset_row, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_OFF_TABLES' )

      end


