C
C+lsf_write_def
C
      SUBROUTINE lsf_write_def ( psf_lun, key, lsf_buffer, s )

C     Writes a given record definition to an LSF file.
C
C     Given:
C         Physical sample file unit number (PSF must be open).
              integer         psf_lun
C         Logical sample file key
              integer         key
C         Logical sample file record
              integer         lsf_buffer(*)

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Writes the given LSF definition to the to the LSF file. If the
C     key is zero then the file is added, otherwise a lsf definiton
C     is searched for and the definiton updated.
C
C     If successful the LSF file is put on the tape save list.
C
C     12/11/98 - LSF key definition changed to allow microsecond
C                resolution.  For detils of the 'time' and 'etime'
C                clock routines, see the SPARCompiler Fortran
C                Reference Manual, Sections 7.8, 7.54 [DJT].
C     12/04/2000 - added io_setacc for new lsf file [GP]
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_record.inc'

C     ****************************************************************
C
C     Local constant declarations
C         Logical sample file record length, in bytes.
              integer         rec_length
              parameter     ( rec_length = lsf_len*4 )

C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         Full file name
              character*80    file_name
C         Logical sample file unit
              integer         lsf_lun
C         Parameters for io_enqfil subroutine call - needed to get the
C         number of bytes in the LSF.
              integer         num_bytes, protn, dates(3)
C         User mode and terminal number returned from io_enqexe
              character*16    user
              integer         mode, term_no
C         Number of records in LSF
              integer         num_recs
C         Record number of LSF
              integer         rec_num
C         Local variables for LSF key timestamp
              real*4          etime, t(2)
              integer*4       time

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call io_enqexe( user, mode, term_no )

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Open logical sample files.
      call io_nxtlun( lsf_lun, s )
      if (s.ne.0) goto 9999
      call enq_namfil( psf_lun, 'LSF', file_name, s )
C      if ( s .ne. 0 ) then
C          write(*,*) 'enq_namfil(file_name,s): ', file_name, s
C      endif
      if (s.eq.0) then
          open(   unit    = lsf_lun,
     *            file    = file_name,
     *            access  = 'DIRECT',
     *            recl    = rec_length,
     *            status  = 'OLD',
     *            iostat  = s             )

C         Now find the number of bytes in the file - hence num. of recs.
          call io_enqfil( file_name, num_bytes, protn, dates, s )
          num_recs = int( num_bytes/rec_length )
      else if (s.eq.NO_FILE.and.key.eq.0) then
          open(   unit    = lsf_lun,
     *            file    = file_name,
     *            access  = 'DIRECT',
     *            recl    = rec_length,
     *            status  = 'NEW',
     *            iostat  = s             )
          call io_setacc(file_name, 'r', 'rw', 'rw', s)
          num_recs = 0
      else
          write(*,*) 'failing for: ',file_name
          s = NO_LSFSAVED
      end if
      if (s.ne.0) goto 9999

      if (key.eq.0) then
C         Copy LSF definition record
          do 100, i = 1, lsf_len
              log_samp_file(i) = lsf_buffer(i)
  100     continue

C         Set logical sample file key to current time
c         call util_enqnow( lsf_key )
          lsf_key = time()+etime(t)*1000000
          lsf_owner = user
          call util_enqnow( lsf_time_created )
          rec_num = num_recs+1
      else
C         Read through file until correct lsf is found.
          rec_num = 1
  200     read( lsf_lun, rec=rec_num, iostat=s) log_samp_file
              if ( key.eq.lsf_key.and.s.eq.0 ) then
C                 Found LSF, so copy new LSF to update it.
                  do 300, i = 1, lsf_len
                      log_samp_file(i) = lsf_buffer(i)
  300             continue
              else
                  rec_num = rec_num+1
              end if
          if (key.ne.lsf_key.and.rec_num.le.num_recs.and.s.eq.0)
     *                                                       goto 200

          if (key.ne.lsf_key) s = ILL_LSF
      end if

      if (s.eq.0) then
          lsf_last_user = user
          call util_enqnow( lsf_last_used )
          write( lsf_lun, rec=rec_num, iostat = s ) log_samp_file
          close(lsf_lun)
          call lsf_stlrec( rec_num, s )
          key = lsf_key
          if (s.eq.0) then
C             Return LSF definition record
              do 400, i = 1, lsf_len
                  lsf_buffer(i) = log_samp_file(i)
  400         continue
          end if
      end if

C     Put the LSF file on the tape save list, if neccessary.
*     call save_sf( file_name, s )

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subr LSF_WRITE_DEF' )
          return
      end
