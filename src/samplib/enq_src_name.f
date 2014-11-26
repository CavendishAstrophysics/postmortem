
C+ENQ_SRC_NAME

       subroutine enq_src_name ( lun, src_num, src_name, s )
C
C     Returns default phase centre information of a source.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C         The number of the source in the sample file.
              integer        src_num

C     Returned:
C         A string giving the source name.
              character*(*)   src_name
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     The information is obtained from the control tables so the
C     source does not have to be open.
C
C     Possible return status's are:
C         ILL_CONTTAB     - Error in control tables.
C         NO_SRCNUM       - No such source. (Error not logged)
C
C-

C     Global includes -
C
      include '/mrao/post/include/cal_record.inc'

      integer     sf_type
      real*8      epoch, ra, dec
      character*64 fname

      if ( s .ne. 0 ) return

      call enq_sftype( lun, sf_type, s )
        inquire(lun,name=fname,iostat=s)
      if ( sf_type .ne. 3 ) then
          call enq_pc_epoch( lun, src_num, epoch, ra, dec, src_name, s )
      else
          call enq_src_def( lun, src_num, cal_record, s )
          src_name = cal_source
      end if

      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_SRC_NAME' )

      end
