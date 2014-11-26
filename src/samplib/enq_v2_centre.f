C+ENQ_V2_CENTRE

       subroutine enq_v2_centre ( num_centre, src_name, sf_name,
     :                         num_samp, ra_ref, dec_ref, centre_num, s)
C
C     Returns the pointing centres associated with a sample file.
C
C     Returned:
C         Number of pointing centres
              INTEGER         NUM_CENTRE
C         Source name for each pointing centre
              CHARACTER*24    SRC_NAME(*)
C         Sample file name for each pointing centre
              CHARACTER*40    SF_NAME(*)
C         Number of samples at each centre (multi-centre observations)
              INTEGER         NUM_SAMP(*)
C         RA, Dec at reference date for each centre
              REAL*8          RA_REF(*)
              REAL*8          DEC_REF(*)
C         Pointing centre index for this sample file
              INTEGER         CENTRE_NUM
C         Status
              INTEGER         S
C
C     This routine returns the details of the pointing centres for the
C     current sample file.
C
C     Control Tables Version 2 support routine for ENQ_CENTRES
C
C     DJT     23 March 1992
C             5 August 1993 (offsets included)
C             24 October 1994 (sample file name included)
*-

      include '/mrao/include/constants.inc'
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'

      real*8    ra, dec
      integer   i, i1, i2

      if ( s .ne. 0 ) return

      num_centre = ncentre
      if (num_centre.eq.1) then
        src_name(1) = source_list(1)
        i1 = index(file_list(1),')')
        if (i1.eq.0) then
          sf_name(1) = file_list(1)
        else
          sf_name(1) = file_list(1)(i1+1:)
        endif
        num_samp(1) = 1
        if (offset.eq.4) then
          call precrd(datref,raref_list(1),decref_list(1),
     :                                           datobs,ra,dec)
          ra = ra - (60.D0*const_sa2r*offset_w)/cos(decdate)
          dec = dec + (60.D0*const_sa2r*offset_n)
          call precrd(datobs,ra,dec,datref,ra_ref(1),dec_ref(1))
        else
          ra_ref(1) = raref_list(1)
          dec_ref(1) = decref_list(1)
        endif
        centre_num = 1
      else
        do i = 1, num_centre
          src_name(i) = source_list(i)
          i1 = index(file_list(i),')')
          if (i1.eq.0) then
            sf_name(i) = file_list(i)
          else
            sf_name(i) = file_list(i)(i1+1:)
          endif
          num_samp(i) = tcentre(i)
          if (offset.eq.4) then
            call precrd(datref,raref_list(i),decref_list(i),
     :                                             datobs,ra,dec)
            ra = ra - (60.D0*const_sa2r*offset_w)/cos(decdate)
            dec = dec + (60.D0*const_sa2r*offset_n)
            call precrd(datobs,ra,dec,datref,ra_ref(i),dec_ref(i))
          else
            ra_ref(i) = raref_list(i)
            dec_ref(i) = decref_list(i)
          endif
        enddo
        centre_num = centre
      endif

c  Convert ND-style sample file names

      do i = 1, num_centre
        i1 = index(sf_name(i),':')
        if (i1.gt.0) then
          i2 = chr_lenb(sf_name(i))
          call chr_chlcas(sf_name(i)(i1+1:i2))
          sf_name(i)(i1:i1) = '/'
        endif
      enddo

      end



