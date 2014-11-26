
C     *****************************************************************
C
C+write_aperture
C
      subroutine write_aperture( file_name, data, comp, s )

C     Writes out the cos, sin, amp or phase of an aperture as a map.
C
C     Given
C         Map file name.
              character*(*)   file_name
C         Aperture data
              complex         data(*)
C         Component type - must match one of 'amplitude', 'phase',
C                          'cosine' or  'sine'.
              character*(*)   comp
C
C     Returned
C         Status - must be zero on entry.
              integer         s
C
C     Writes out a single component (sine, cosine, amplitude or phase)
C     or both components (as a complex aperture in mapper format)
C     of the current aperture data in the form of a map. This enables
C     the aperture to be displayed using all the available map display
C     routines.
C
C     NPR     7 November 1987, from a previous version from last year.
C
C-
C     ****************************************************************
C
C     Function declarations -
C
      include    '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
C
      include        '/mrao/include/maplib_redtape.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         File i/o system parameters
              integer     rec_siz
              parameter  (rec_siz  = 4096)

C     Variables, equivalences and commons
C         Output record.
              real        out_rec(rec_siz)
C         Number of records in map, current record and current element.
              integer     n_recs, rec_n, rec_index
C         First and last row in record and number of rows in record.
              integer     first_row, last_row, rows_in_rec
C         The column and row of the current element in the map
              integer     col_n, row_n
C         The column and row of the current element in the aperture
C         and its offset from the first element in the aperture.
              integer     uv_col_n, uv_row_n, offset
C         Length of file block=length of map row in words.
              integer     block_siz
C         First block where map data starts, and current block number.
              integer     init_block_n, block_n
C         Flag set if we are on the congagated side of the aperture.
              logical     cong_flg
              real        sgn
C         Flags set if outputting cos, sin, amp or phase of the aperture
              logical     cosap, sinap, ampap, phiap
C         File names
              character   map_name*16, user*33, type*4, aper_name*80
C         String length and unit number of the aperture
              integer     ls, lun

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      cosap = chr_cmatch( comp, 'cosine' )
      sinap = chr_cmatch( comp, 'sine' )
      ampap = chr_cmatch( comp, 'amplitude' )
      phiap = chr_cmatch( comp, 'phase' )
      call io_brkfil( file_name, user, map_name, type )
      if (cosap) then
          call io_makfil( user, map_name, 'cos', aper_name, ls )
      else if (sinap) then
          call io_makfil( user, map_name, 'sin', aper_name, ls )
      else if (ampap) then
          call io_makfil( user, map_name, 'amp', aper_name, ls )
      else
          call io_makfil( user, map_name, 'phi', aper_name, ls )
      end if
      call opemap( lun, aper_name, 'write', 1, s )


      n_recs       = (iymax*ixmax-1)/rec_siz + 1
      rows_in_rec  = min(iymax, int(rec_siz/ixmax))
      init_block_n = mpblk1
      block_siz    = mpblk/4
      block_n      = init_block_n
      zmax         = 0.0
      zmin         = 0.0

C     ****************************************************************
C
C         Main Code
C         ---------
C
C
      last_row = 0

      do 1000 rec_n = 1, n_recs
          rec_index = 0
          first_row = last_row+1
          last_row  = min(first_row+rows_in_rec-1, iymax)

C         For all rows in that record...
          do 800 row_n = first_row, last_row
C             For each column...
              do 500 col_n=1,ixmax
                  cong_flg=col_n.gt.(ixmax/2+1)
                  if (cong_flg) then
                      uv_col_n=ixmax-col_n+2
                      uv_row_n=iymax-row_n+2
                      sgn = -1
                  else
                      uv_col_n=col_n
                      uv_row_n=row_n
                      sgn = 1
                  end if

                  offset   = (ixmax/2+1)*(uv_row_n-1)+uv_col_n
                  rec_index= rec_index+1
C
                  if ( uv_row_n.gt.iymax .or.
     *                data(offset).eq.(0.0,0.0)) then
                      out_rec(rec_index)=0.0
                  else
                      if (ampap) then
                          out_rec(rec_index)=cabs(data(offset))
                      else if (cosap) then
                          out_rec(rec_index)=real(data(offset))
                      else if (sinap) then
                          out_rec(rec_index)=sgn*aimag(data(offset))
                      else if (phiap) then
                          out_rec(rec_index)=sgn*
     *                      atan2(imag(data(offset)),real(data(offset)))
                      end if
                  end if

                  if (out_rec(rec_index).gt.zmax) then
                      zmax   = out_rec(rec_index)
                      ivzmax = iymax/2-row_n+1
                      iuzmax = col_n-ixmax/2-1
                  end if
                  if (out_rec(rec_index).lt.zmin) then
                      zmin   = out_rec(rec_index)
                      ivzmin = iymax/2-row_n+1
                      iuzmin = col_n-ixmax/2-1
                  end if
  500         continue
  800     continue
C
          call io_wrfile(lun, block_n, out_rec, rec_index, s)
          if (s .ne. 0) then
              goto 9999
          else
              block_n=block_n+rec_index/block_siz
          end if
 1000 continue

      call wrredt( lun, 1, s )
      if (s .ne. 0) goto 9999
      close( lun )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C

 9999 continue
          call map_wrerr( s, 'in subroutine WRITE_APERTURE' )
          return
      end
