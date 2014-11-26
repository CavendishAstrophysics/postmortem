C
C+lsf_open
C
      SUBROUTINE lsf_open ( name,
     *                      key,
     *                      sf_access,
     *                      lsf_num,
     *                      s                      )

C
C     Opens a logical sample file.
C
C     Given:
C         Access code to open the sample file with.
              character*(*)   sf_access

C     Updated:
C         Physical sample file name.
              character*(*)   name
C         Logical sample file key
              integer         key

C     Returned:
C         Logical sample file number - used when calling other routines
              integer         lsf_num
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Opens the logical sample file associated with the given sample
C     file and having the key given by the 'key' parameter.
C
C     The physical sample file is opened with the access given by
C     the 'access' string - the other associated sample files are only
C     opened if they are needed, and they are always opened 'RC' (they
C     can be opened for write on a different unit if necessary).
C
C     Valid logical sample file keys are of the following form:
C     -1    - The default LSF for a given physical sample file. These
C             can be opened non-interactively.
C      0    - Illegal - only used as an input parameter, never returned.
C      1    - The current LSF in the common blocks, one that has been
C             edited but not saved.
C     other - The NORD file system date of when the logical sample file
C             was saved.
C
C     The input values of 'name' and 'key' are treated in the following
C     way:
C
C     1.  name = any, key = not -1, 0 or 1
C         Calls open_sf to establish a valid sample file name, and opens
C         the saved lsf for that sample file that has the given key.
C     2.  name = any, key = -1
C         Calls open_sf to establish a valid sample file name, and opens
C         the default logical sample file for that sample file. Note
C         that if the sample file has more than one source then there
C         has to be interaction to ascertain which source to use.
C     3.  name = any, key = 0
C         Calls open_sf to establish a valid sample file name, then asks
C         user to select an existing, saved lsf for that sample file.
C     4.  name = ' ', key = 1
C         Opens the lsf definition currently in the common blocks, if it
C         exists. Otherwise calls open_sf and opens the default lsf.
C     5.  name = 'text', key = 1
C         If the full expanded sample file name in the common blocks is
C         the same as the given one, then this is opened. Otherwise, it
C         is the same as 2 above.
C
C     If the open is successful then the value of 'name' returned is
C     the expanded, verified sample file name and the value of 'key'
C     is the one selected. A 'key' value of '1' is returned only if no
C     other value is available.
C
C     If the open is not successful then the return status will be
C     non-zero and the values returned reflect the lsf currently in the
C     common blocks.
C
C     Possible return status's:
C     NO_LSFSAVED - opening with a key of zero, but no LSF's are saved.
C     ILL_LSF     - Error in the lsf definition or trying to do an
C                   interactive open non-interactively.
C     Other       - Unexpected io_system error.
C
C     NPR     15 December 1987.
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
      include  '/mrao/post/include/lsf_definition.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         Values of the special current and default lsf keys.
              integer         current_key, default_key
              parameter     ( current_key =  1 )
              parameter     ( default_key = -1 )


C     Variables, equivalences and commons
C         Save buffer for existing lsf.
              integer         buffer( lsf_len )
C         Loop counter and record number counter
              integer         i
C         Sample file logical unit numbers
              integer         sf_lun
C         Full file name for root sample file and file of lsf's.
              character*80    sf_name
C         Key of current LSF on entry
              integer         old_key

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Initialise unit numbers.
      sf_lun  = 0

C     Save existing definition
      do 10, i = 1, lsf_len
          buffer(i) = log_samp_file(i)
   10 continue

      call lsf_enq_lsf( 0, sf_name, old_key, s )

C     ****************************************************************
C
C         Main Code
C         ---------
C

C     Establish the sample file to use.
      if ((key.eq.current_key) .and. (chr_lenb(name).eq.0)) then
         name = sf_name
      endif

C     Open physical sample file.
      call open_sf( sf_lun, name, sf_access, 0, s )
      if (s .ne. 0) goto 9999

C     Change the key to open the default lsf if there is no current one.
      if ((key.eq.current_key) .and. .not.chr_chsame(sf_name,name))
     *    key = default_key

C     Open logical sample file.
      if (key .eq. default_key) then
C         Open the default lsf
          call init_lsf_desc( sf_lun, s )
      else if (key .eq. current_key) then
C         Open the current lsf
          call open_source( sf_lun, src_num, 0, s )
      else
          call lsf_read_def( sf_lun, key, log_samp_file, s )
          call open_source( sf_lun, src_num, 0, s )
      end if

C     Set up LSF for runtime use (open sources, set up commons etc.)
      call init_runtime_lsf( sf_lun, lsf_num, s )

C     Initialise the model visibilities definition.
      call lsf_init_model( lsf_num, s )
      if ( s .ne. 0 ) goto 9999

      call lsf_enq_lsf( 0, name, key, s )
      if ( s .ne. 0 ) goto 9999

C     Force the re-calculation of the flagging array from the flag table
      call flag_docalc( key, .true. )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
C         Allow calling programs to deal with the problem of there
C         being no lsf's saved.
          if ( s .ne. NO_LSFSAVED .and. s .ne. USR_BREAK ) then
              call lsf_wrerr( s, 'in subroutine LSF_OPEN' )
          end if

C         Restore previous logical sample file defaults.
          do 9010 i = 1, lsf_len
              log_samp_file(i) = buffer(i)
 9010     continue
          i = 0
          call lsf_enq_lsf( 0, name, key, i )

C         Close any open files.
          if (sf_lun .ne. 0)  call close_sf( sf_lun, i )
          sf_lun = 0

C         Setup flag table if required
          call flag_docalc( key, .true. )

          return
      end
