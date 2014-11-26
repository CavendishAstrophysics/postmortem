C+map_chredt
C
      SUBROUTINE map_chredt ( s )

C     Validates the mapping section of the current map redtape.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Status ILL_REDTAPE is returned if there is no mapping redtape
C last mod 30 May 2000 (GP)    
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/maplib_errors.inc'
      include    '/mrao/include/maplib_redtape.inc'
      include    '/mrao/include/maplib_tabfns.inc'
      include    '/mrao/post/include/grading_types.inc'
      include    '/mrao/post/include/weighting_types.inc'

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ((maptyp.lt.1 .or. maptyp.gt.3)          .or.
     *    (convtp.lt.1 .or. convtp.gt.num_conv)   .or.
     *    .not.((gradtp.ge.0   .and. gradtp.le. num_grad)   .or.
     *          (gradtp.ge.100 .and. gradtp.le.(num_grad+100)))  .or.
     *    (numlsf.lt.1 .or. numlsf.gt.maxlsf)     .or.
     *    .not.((wghttp.ge.0 .and.wghttp.le. num_weight)    .or.
     *          (wghttp.ge.10.and.wghttp.le.(num_weight+10)).or. 
     *          (wghttp.ge.20.and.wghttp.le.(num_weight+20))    ))
     *                                                   s = ill_redtape

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine MAP_CHREDT' )
          return
      end
