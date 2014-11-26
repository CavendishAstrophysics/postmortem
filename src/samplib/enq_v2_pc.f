

C+ENQ_V2_PC

       subroutine enq_v2_pc ( src_num, sf_epoch, ra, dec, src_name, s )
C
C     Returns default phase centre information from control tables.
C
C     Given:
C         The number of the source in the sample file.
              integer        src_num
C
C     Returned:
C         The epoch, ra and dec of the observation.
              real*8          sf_epoch, ra, dec
C         A string giving the source name.
              character*(*)   src_name
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Control tables version 2 support routine for ENQ_PC_EPOCH.
C-

C     Global includes -
C
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

C     Normal sample file with one source
      if ( src_num .gt. Ncentre ) then
          s = NO_SRCNUM
      else
          sf_epoch = datobs
          ra  = RAdate
          dec = DECdate
          src_name = source
      end if

      end
