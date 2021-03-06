C
C+lsf_list_def
C
      SUBROUTINE lsf_list_def ( psf_lun, s )

C     Lists record definitions in an LSF file.
C
C     Given:
C         Physical sample file unit number (PSF must be open).
              integer         psf_lun

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     The LSFs for the given physical sample file are listed on the
C     output device.
C
C     Possible return status's:
C     Other       - Unexpected system error.
C
C     PA     11 April   1990
C     PA     20 March   1991 ; Added support for deleted LSF's
C     GP     28 Sep 1999   more digits in lsf numbers; shorter dates
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'

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
C     Local constant and variable declarations
C         Buffer for LSF record
              integer         buffer_record(lsf_len)
C         Loop counter and record number counter
              integer         i, next_rec
C         Logical sample file logical unit number
              integer         lsf_lun
C         Full file name for file of lsf's.
              character*80    lsf_fname
C         General purpose string and length
              character*160   string
              integer         ls

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      lsf_lun = 0
      do i=1,lsf_len
        buffer_record(i) = log_samp_file(i)
      end do

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     List the saved lsf's
      call enq_namfil( psf_lun, 'LSF', lsf_fname, s )
      if ( s .eq. NO_FILE ) then
        call io_wrout('No LSFs are saved for the sample file')
        s = 0
        return
      end if

      call io_nxtlun( lsf_lun, s )
      open(   unit    = lsf_lun,
     *        file    = lsf_fname,
     *        access  = 'DIRECT',
     *        recl    = lsf_len*4,
     *        status  = 'OLD',
     *        iostat  = s             )
      if (s .ne. 0) goto 9999

C     Read through file
      read( lsf_lun, rec=1, iostat = s ) log_samp_file
      next_rec = 2
      if ( s .ne. 0 ) goto 9999

  100 continue
         if (lsf_version.ge.0) then
              string = ' '
              write(string,'(A,I4,A)') 'LSF ',(next_rec-1),' ('
              ls = chr_lenb(string)
              call util_extdat( lsf_time_created, 0, string(ls+1:), i )
              string(ls+i+1:) = ') '//lsf_name(1:chr_lenb(lsf_name))
* test:
              ls = chr_lenb(string)
              write(string(ls+1:),'(a, i12)') ' key ', lsf_key

              ls = chr_lenb(string)
              call io_wrout(string(1:ls))
         end if

         read(lsf_lun,rec=next_rec,iostat=s) log_samp_file
         next_rec = next_rec+1

      if ( s .eq. 0 ) goto 100
      s = 0
      call io_wrout(' ')

C     Copy back
      do i = 1, lsf_len
         log_samp_file(i) = buffer_record(i)
      end do
      close( lsf_lun, iostat = s )
      lsf_lun = 0
      if ( s .ne. 0 ) goto 9999


C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. 0) then
              call lsf_wrerr( s, 'in subroutine LSF_LIST_DEF' )
          end if

          if (lsf_lun .ne. 0) close( lsf_lun )
      end
