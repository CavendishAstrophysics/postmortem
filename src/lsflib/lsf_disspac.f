C
C+lsf_disspac
C
      SUBROUTINE lsf_disspac ( lsf_num,
     *                         s                      )

C
C     Displays a representation of the spacings present in the LSF
C
C     Given:
C         Logical sample file number
              integer             lsf_num

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Function declarations

      include  '/mrao/include/chrlib_functions.inc'
      logical         util_tstbit

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         String and length
              character*72    string
              integer         ii
C         Logical unit number of output device
              integer         lun
C         Number of visibilities in the file
              integer         num_vis
C         Number of aerials spacings, sub-bands, channels
              integer         nae, nsp, nsb, nch
C         Identification for individual aerials etc.
              integer         iae, iae1, iae2, isp, isb, ich
C         Number of pages in output
              integer         npage
C         Sub-band identifications
              character*1     sub_bands(5)
              data            sub_bands / 'A','B','C','D','E' /


C Subroutine initialisation
C -------------------------
C
C Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

      call io_enqout( lun )


C Main Code
C ---------


C Find number of visibilities and structure of the telescope
      call enq_numvis( sf_lun, num_vis, s )
      call enq_obsdef( sf_lun, nae, nsp, nsb, nch, s)
      if (nae.gt.8) then
        call io_wrout(
     *             '*** Facility only implemented for RT sample files')
        return
      end if

C loop for two pages of output
      do npage=1,2
C .. output header for each page
      call io_wrpage( .true., s )
      write( lun,'(1X,''Aerial  '',''1'',8X,''2'',8X,''3'',8X,''4'',
     *                          8X,''5'',8X,''6'',8X,''7'',8X,''8'')' )
      write( lun,'(1X,'' SB CH  '',''12345678 '',''12345678 '',
     *                             ''12345678 '',''12345678 '',
     *                             ''12345678 '',''12345678 '',
     *                             ''12345678 '',''12345678 '')' )
C .. move down the page at the rate of one aerial/sub-band and note all
C    spacings with this aerial/sub-band
      do nae=4*(npage-1)+1,npage*4
        do nsb=1,5
          string = ' '
          do i=1,num_vis
            if (util_tstbit(sp_bit_arr,i)) then
              call enq_vis_desig( sf_lun, i, isp, isb, ich, s )
              call enq_ae_vis(    sf_lun, i, iae1, iae2, s )
              if (iae1.eq.nae .or. iae2.eq.nae) then
                if (isb.eq.nsb) then
                   iae = iae2
                   if (iae2.eq.nae) iae = iae1
                   ii = (iae-1)*9 + ich
                   string(ii:ii) = '.'
                end if
              end if
            end if
          end do
          write( lun, '(1X,I1,A1,6X,A72)' ) nae, sub_bands(nsb), string
        end do
      end do

C .. supply page prompt
      string = ' '
      if (s.ne.0) goto 9999
      call io_getstr('.. press RETURN to continue',' ',string,s)
      if (s.ne.0) then
        s = 0
        goto 100
      end if

      end do

      if (s.ne.0) goto 9999
100   continue

      return

C
C Error Handling
C --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_DISPAC' )
          return
      end
