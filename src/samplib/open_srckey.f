

C+OPEN_SRCKEY

      subroutine open_srckey ( lun, src_key, buff_size, src_num, s )
C
C     Keyed open for a source in a given, open, sample file
C
C     Given:
C         Logical unit number of sample file.
              integer     lun
C         Key to source in sample file.
              integer     src_key
C         Buffer size in pages (2048 bytes)
              integer     buff_size
C
C     Returned:
C         Source number to be used for accessing.
              integer     src_num
C         Status - must be zero on entry
              integer     s
C
C     This open is an interface to the open_source routine, but it uses
C     the source key as a key, rather than its physical position in
C     the sample file. Thus it should be used in database applications
C     because it is independent of the structure of the sample file (It
C     will continue to work after a source lower down in the sample file
C     has been deleted).
C
C     Otherwise, it is the same as open_source and the specifications
C     for both routines are the same.
C
C     S = 0 for successful return, otherwise errcode.
C
C     NPR,    October 1988
C-
C
      include '/mrao/post/include/cal_record.inc'
      include '/mrao/post/include/remove_record.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      integer     num_src, sf_type, key

      if (s.ne.0) return

      call read_ct( lun, s )
      call enq_sftype( lun, sf_type, s )

      if (sf_type.eq.1) then
C         Physical sample file - return only source
          src_num = 1
      else if (ct_vers.ne.0) then
          call enq_numsrc( lun, num_src, s )
          if (num_src.gt.0) then
              src_num = 0
  10          continue
                  src_num = src_num+1
                  if (sf_type.eq.2) then
                      call enq_src_def( lun, src_num, remove_record, s )
                      key = rem_key
                  else
                      call enq_src_def( lun, src_num, cal_record, s )
                      key = cal_key
                  end if
              if (key.ne.src_key.and.src_num.lt.num_src) goto 10

              if (key.ne.src_key) s = NO_SRCNUM
          else
              s = NO_SRCNUM
          end if
      else
          s = ILL_CONTTAB
      end if

      call open_source( lun, src_num, buff_size, s )
      if (s .ne. 0) goto 999
      return

 999  call smp_wrerr( s, 'in subroutine OPEN_SRCKEY' )

      end
