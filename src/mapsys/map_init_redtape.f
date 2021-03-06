C+map_init_redtape
C
      subroutine map_init_redtape (phys_sf_name, lsf_key, s)
C
C     Initialises all the map redtape to reasonable defaults.
C
C     Given:
C         Physical sample file name
              character*(*)   phys_sf_name
C         Logical sample file key
              integer         lsf_key

C     Returned:
C         Status value, zero if success - must be zero on entry.
              integer         s
C
C     Subroutine to construct default red tape for a map file using
C     the single logical sample file specified.
C
C     Possible status values returned :
C         0           - Success.
C         NO_LSFSAVED - No logical sample files saved for this sample
C                       file - no error is logged as this should be
C                       handled by the calling programs.
C         Other       - system or MAPLIB error numbers
C-
C     ****************************************************************

* last mod GP 14 Feb 2002 (beamwidths)

C     Function declarations

      include '/mrao/include/chrlib_functions.inc'

Common blocks, parameters 

      include '/mrao/include/constants.inc'
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/maplib_redtape.inc'
      include '/mrao/include/maplib_tabfns.inc'
      include '/mrao/post/include/mapsys_save.inc'
      include '/mrao/post/include/lsflib_errors.inc'
      include '/mrao/post/include/grading_types.inc'
      include '/mrao/post/include/weighting_types.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants - various map defaults.
C         Map size.
              integer         x_size, y_size
              parameter      (x_size = 256)
              parameter      (y_size = 256)
              integer         data_type
              parameter      (data_type = 3)

C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         General purpose string and two string length indicators
              character       string*40
              integer         ls
C         Logical sample file number and sample file unit and source
              integer         lsf_num, sf_unit, src_num
C         U sampling in arcsec/gridpoint.
              real*8          u_samp
C         Values of lsf variables returned by lsf enquiry routines.
              real*8          ref_ra, ref_dec, ref_date
              character*(16)  lsf_source
              integer         lsf_numsp
              real            lsf_maxsp_wlen
C         Values of control table variables returned by enquiry routines
              character*9     sf_units
              real*8          sf_freq, sf_epoch,
     *                        sf_ra, sf_dec,
     *                        sf_ra_aes, sf_dec_aes,
     *                        sf_ra_comp, sf_dec_comp
              integer         sf_poln
              integer         sf_tscope
              real*4          rt_freq
              real*4          beam_widths(2)

C     ****************************************************************
C
C     Function initialisation

      if  (s .ne. 0) return
      call dpredt (mapsys_save, s)


C     Open the logical sample file to be used to make the map
      call lsf_open (phys_sf_name, lsf_key, 'READ', lsf_num, s)
      if  (s .ne. 0) goto 9999

C     Call control table enquiry routines.
      call lsf_enq_sf      (lsf_num, sf_unit, src_num, s)
      call lsf_enq_numsp   (lsf_num, lsf_numsp, s)
      call lsf_enq_max_rad (lsf_num, lsf_maxsp_wlen, s)
      call lsf_enq_pc_rdate(lsf_num,
     *                      ref_date, ref_ra, ref_dec, lsf_source, s)
      call enq_units    (sf_unit, sf_units, s)
      call enq_freq     (sf_unit, sf_freq, s)
      call enq_poln     (sf_unit, sf_poln, s)
      call enq_point    (sf_unit, sf_ra_aes, sf_dec_aes, s)
      call enq_path_comp(sf_unit, sf_ra_comp, sf_dec_comp, s)
      call enq_pc_epoch (sf_unit, src_num,
     *                   sf_epoch, sf_ra, sf_dec, string, s)
      call enq_tscope   (sf_unit, string, sf_tscope, s)

C     Close the logical sample file
      i = 0
      call lsf_close (lsf_num, i)
      if  (i .ne. 0 .and. s .eq. 0) s = i
      if  (s .ne. 0) goto 9999

C default sampling (arcsec)  (oversampling = 1.5)
      u_samp = 1.0D+0 / (const_sa2r*2.0D+0*lsf_maxsp_wlen*1.5D+0)

* beamwidths (arcsec): 1.5*(lambda/2D), 1.5*(lambda/2D)*cosec(dec)

        beam_widths(1) = 0.75/(const_sa2r*lsf_maxsp_wlen)
        beam_widths(2) = beam_widths(1)/sin(sf_dec)

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     call MAPLIB redtape setting routines.
      call nwredt (x_size, y_size, data_type, sf_tscope,s)
      call stmapj  (1, u_samp, 0.0D+0, sf_epoch, 0.0D+0, s)
      call stmapc (ref_ra, ref_dec, ref_date, sf_epoch, lsf_source, s)
      if  (s .ne. 0) goto 9999


C     Section 0 - Redtape format
C     --------------------------

C     All except for maptyp and istynd is set by call to nwredt
      istynd = 1
      maptyp = 1


C     Section 1 - Character title.
C     ----------------------------

      if (sf_tscope.eq.1) then
        call adredt ('Instrument', 'CLFST 151MHz', s)
      else if (sf_tscope.eq.2) then
        call adredt ('Instrument', 'CLFST 38MHz', s)
      else if (sf_tscope.eq.3) then
        call adredt ('Instrument', 'VLB 81.5MHz', s)
      else if (sf_tscope.ge.4 .and. sf_tscope.le.7) then
        call adredt ('Instrument', 'RYLE telescope', s)
      end if

C     Source title set by call to stmapc
      write (string, ' (F8.2, '' MHz.'')') sf_freq/1.0D+6
      call adredt ('Frequency', string, s)
      call adredt ('Polarisation', 'Uncertain...', s)
      write (string, '(I4)') lsf_numsp
      call adredt ('Spacings', string, s)
C     Map size set by call to nwredt
C     Projection set by call to stmapj
C     Observation date set by call to stmapc
      call chr_chdtos (sf_ra/const_h2r,  0, string, ls)
      call chr_chdtos (sf_dec/const_d2r, 0, string(ls+1:), ls)
      i = chr_intlc (string)
      ls = chr_lenb (string)
      if (string(i:i) .eq. '-') then
          write(rtitle(9),'(F6.1,2A)')
     *            obsdat, ' obs centre :HA',string(i+1:ls)
      else
          write(rtitle(9),'(F6.1,2A)')
     *            obsdat, ' obs centre :  ',string(i:ls)
      end if
C     Map centre set by call to stmapc
C     Map created set by call to nwredt


C     Section 2 - Computing redtape
C     -----------------------------

C     Entirely set by call to nwredt


C     Section 3 - astronomical redtape
C     --------------------------------

      rt_freq = sf_freq/1.0E+6
      call sttype (rt_freq, sf_poln,
     *             'Flux Density    ',
     *              sf_units, s)
      exunit = sf_units
      zerol  = 0.0
      scalef = 1.0
      zmax   = 0
      iuzmax = 0
      ivzmin = 0
      zmin   = 0
      iuzmin = 0
      ivzmin = 0
C     iproj set by call to stmapj
      nspac  = lsf_numsp


C     Section 4 - Mapping function definitions.
C     -----------------------------------------

      convtp    = l2_optimal
      convos    = 100
      convhw    = 3
      convpa(1) = 0.45
      convpa(2) = 0.0
      corrtp    = conv_fft
      wghttp    = radial_wt
      wghtpa(1) = 0.0
      wghtpa(2) = 0.0
      gradtp    = gaussian_gr
      gradpa(1) = 1.0/(1.5*sqrt(2.0*log(1.0/0.30)))
      gradpa(2) = 0.0


C     Section 5 - Derived quantities 
C     ------------------------------

* attempt to get CLEAN to make sensible guess [14 2 2002: GP]

        call stbeam(1.0, beam_widths, 0.0, s)


C     Section 6 - astrometric and mapper redtape
C     ------------------------------------------

C     Set map centre
C     ramap, decmap, refdat set by call to stmapc
C     xmc and ymc set by call to nwredt
      skew   = 0.0
C     usamp, vsamp, epoch and prang set by call to stmapj
C     raobs, decobs set by call to stmapc
      rapnt  = sf_ra_aes
      decpnt = sf_dec_aes
C     obsdat set by call to stmapc
      rapc   = sf_ra_comp
      decpc  = sf_dec_comp
      rampc  = ramap
      decmpc = decmap


C     Section 7 is in a state of flux at the moment because of the
C     advent of logical sample files and so it is not filled in.
C     The only part of section 8 filled in is done by nwredt.


C     Section 9 - Logical sample files used.
C     --------------------------------------

C     Put in the details for the current logical sample file.
      numlsf = 1
      maxlsf = 10
      call stmlsf (numlsf, phys_sf_name, lsf_key, s)

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C

 9999 continue
C         Don't print error message if the error is NO_LSFSAVED
C         Let the calling program handle it.
          if (s .ne. NO_LSFSAVED .or. s .ne. USR_BREAK) then
              call map_wrerr (s, 'in subroutine MAP_INIT_REDTAPE')
          end if
          call ldredt (mapsys_save, 0)
          return
      end
