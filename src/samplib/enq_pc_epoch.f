

C+ENQ_PC_EPOCH

       subroutine enq_pc_epoch ( lun, src_num,
     *                           sf_epoch, ra, dec, src_name, s )
C
C     Returns default phase centre information at the observation epoch.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C         The number of the source in the sample file.
              integer        src_num

C     Returned:
C         The epoch, ra and dec of the observation.
              real*8          sf_epoch, ra, dec
C         A string giving the source name.
              character*(*)   src_name
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     The values of ra and dec returned are the actual values that the
C     sample file is centred on. If the ra is negative then the phase
C     centre is an hour angle and moves in ra throughout the file.
C
C     The information is obtained from the control tables so the
C     source does not have to be open.
C
C     Possible return status's are:
C         ILL_CONTTAB     - Error in control tables.
C         NO_SRCNUM       - No such source.
C
C-

C     Global includes -
C
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/remove_record.inc'

      integer     sf_type

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      call enq_sftype( lun, sf_type, s )
      if (ct_vers .eq. 0 .and. sf_type .eq. 1 ) then
C         Normal sample file with one source
          if ( src_num .ne. 1 ) then
              s = NO_SRCNUM
          else
              sf_epoch = datobs
              ra  = ramc
              dec = decmc
              src_name = title
          end if
      else if (ct_vers .eq. 0 .and. sf_type .eq. 2) then
C         Old remove file
          if ( src_num .gt. nrem .or. src_num .le. 0 ) then
              s = NO_SRCNUM
          else
              sf_epoch = datobs
              ra  = rem( 1, src_num )
              dec = rem( 2, src_num )
              src_name = rsource( src_num )
          end if
      else if (ct_vers .eq. 1 .and. sf_type .eq. 2) then
C         New remove file (version 1)
          call enq_v1_epoch( sf_epoch, s )
          call enq_src_def( lun, src_num, remove_record, s )
          call precrd2( 1, rem_refdat,rem_ra,rem_dec, sf_epoch,ra,dec )
          src_name= rem_source
      else if (ct_vers .eq. 2 .and. sf_type .eq. 2) then
C         New remove file (version 2)
          call enq_v2_epoch( sf_epoch, s )
          call enq_src_def( lun, src_num, remove_record, s )
          call precrd2( 1, rem_refdat,rem_ra,rem_dec, sf_epoch,ra,dec )
          src_name= rem_source
      else if (ct_vers .eq. 2) then
C         New 5km sample file
          call enq_v2_pc( src_num, sf_epoch, ra, dec, src_name, s )
      else
          s = ILL_CONTTAB
      end if

      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_PC_EPOCH' )

      end
