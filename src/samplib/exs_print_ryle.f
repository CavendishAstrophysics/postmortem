C
C+
       SUBROUTINE EXS_PRINT_RYLE (OPTION, STATUS)
C      ------------------------------------------
C
C  Prints observing parameters for RYLE sample files.
C
C  The STATUS value should be zero on entry.
C
C 12 March 99; 13 Apr 2000, 4 June 2002, 20 Sep 2002, 28 Jun 04  GP
*-
       CHARACTER  OPTION*(*)
       INTEGER    STATUS
C
       CHARACTER  STRING*60
       CHARACTER*16  chraB, chdecB, chraJ, chdecJ
       character  CDATE*11, LTIME*3
       character*8 text
*       REAL*8     RAPNT, DECPNT, RA, DEC
       real*8     r1950, d1950, r2000, d2000, dr1950, dd1950
       INTEGER    IDATE(3), IPRINT, iunit, PRSEC
       INTEGER    i, LC, ld, j, lrB, ldB, lrJ, ldJ
C
       include '/mrao/include/constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/offset_types.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(iunit)
C
C  Print observation title
C
       write(iunit,*)
       IF (OPTION.EQ.'TITLE') THEN
         IPRINT=1
         LTIME='UT'
         IDATE(1)=IDAT1(1)
         IDATE(2)=IDAT1(2)
         IDATE(3)=IDAT1(3)
         CALL chr_chdate(IDATE,IPRINT,CDATE,LD)
         IF (IUTIM.EQ.1) LTIME='BST'
         write(iunit,1)SOURCE(1:16),ITIM1(3),ITIM1(2),LTIME,CDATE
         write(iunit,2)NSP,NSAMP,FREQ
         DO I=1,5
           LC=CHR_LENB(COMMENT_LINES(I)(1:76))
           IF (LC.GT.0) THEN
             IF (I.EQ.1) write(iunit,*)
             write(iunit,'(2X,A)')COMMENT_LINES(I)(1:LC)
           ENDIF
         ENDDO
    1    FORMAT(2X,A,'started',2X,I2,'.',I2.2,X,A,2X,A)
    2    FORMAT('  File contains',I5,' spacings and',I6,' samples'/
     :          '  1st LO frequency',F8.1,' MHz')
C
C  Print observation parameters
C
       ELSEIF (OPTION.EQ.'PARAMETERS') THEN
C
         write(iunit,'(2X,A,2X,A)')
     :    'Source              :',SOURCE(1:CHR_LENB(SOURCE))
         write(iunit,'(2X,A,2X,A)')
     :    'Title               :',TITLE(1:CHR_LENB(TITLE))
         write(iunit,'(2X,A,2X,A)')
     :    'Observer            :',OBSERVER
         write(iunit,'(2X,A,4X,I6)')
     :    'Number of samples   :',NSAMP
         write(iunit,'(2X,A,4X,I6)')
     :    'Number of spacings  :',NSP
         write(iunit,'(2X,A,3X,F7.1,A)')
     :    '1st LO frequency    :',FREQ,' MHz'
         write(iunit,'(2X,A,6X,I4,A)')
     :    'Integration time    :',INTEGRATION,' secs'
         write(iunit,'(2X,A,3X,F7.1,X,A)')
     :    'Amplitude cut-off   :',ACHOP*AMPSCL,
     :                               AUNITS(1:CHR_LENB(AUNITS))
         write(iunit,'(2x,a,3x,f8.1)')
     :    'Reference date      :', datref

C
C    phase centre(s)
C
         PRSEC=3

        write (iunit, *)
        write (iunit, *) '    phase centre',
     * '              B1950           ',
     * '              J2000'
        do i = 1, Ncentre

            if      (abs(datref - 1950.0) .lt. 1) then
                    r1950 =  raref_list(i)
                    d1950 = decref_list(i)
                    call sla_FK45Z (r1950, d1950, datobs, r2000, d2000)
            else if (abs(datref - 2000.0) .lt. 1) then
                    r2000 =  raref_list(i)
                    d2000 = decref_list(i)
                    call sla_FK54Z (r2000, d2000, datobs, r1950, d1950,
     *                                                   dr1950,dd1950)
            else
                write (iunit, *) 'unexpected reference date', datref
            endif
            call chr_chdtos (r1950/const_h2r, prsec, chraB,  lrB)
            call chr_chdtos (d1950/const_d2r, prsec, chdecB, ldB)

            call chr_chdtos (r2000/const_h2r, prsec, chraJ,  lrJ)
            call chr_chdtos (d2000/const_d2r, prsec, chdecJ, ldJ)




            write (iunit, '(x, 4x, a, x, a, x, a, x, a, x, a)')
     $          source_list(i)(1:12),
     $          chraB(1:lrB), chdecB(1:ldB),
     $          chraJ(1:lrJ), chdecJ(1:ldJ)

        enddo

        if (Ncentre .gt. 1) then

        write (iunit, *)
        write (iunit, *) '    source      ',
     *                   '    samples     ',
     *                   '    file-name'

        do i = 1, Ncentre

            write (iunit, '(x, 4x, a, 4x, i5, 4x, a)')
     $          source_list(i)(1:12), Tcentre(i),
     $          file_list(i)(1:chr_lenb(file_list(i)))

        enddo
        endif
         write (iunit, *)


* offset run?
*         call enq_off_tables(sf_lun, offset, offset_aerial,
*     #                     offset_angle, offset_time,
*     #                     ha_5point, dec_5point,
*     #                     offset2Nx, offset2Ny,
*     #                     offset_zero, offset_row, status)

         if (offset .ne. 0) then
                 text = '        '
                   do j = 1,8
                   if (offset_aerial(j) .eq. 1) then
                      text(j:j) = char(ichar('0')+j)
                   endif
                 enddo


                if       (offset .eq. o_5point) then

                        write (iunit, '(1x,a,f4.1,3a)')
     #                          '5-point offset: angle ',
     #                           offset_arcmin, ' arcmin ',
     #                          '  aerials ', text
                else if  (offset .eq. o_raster) then

                        write (iunit, '(1x,a,a)')
     #                          'rectangular raster, aerials ',text
                        write (iunit,
     #                         '(1x,a,i3,a,i3,a,f5.1,a,a,i3,a,i3,a)')
     #                          'grid ',2*offset2Nx+1,
     #                          ' x ',  2*offset2Ny+1,
     #                          '  step ', offset_arcmin, ' arcmin',
     #                          ' 1st row: ',offset_row,
     #                          '   dwell: ',offset_time, ' samples'

                else if (offset .eq. o_7point) then
                        write (iunit, '(1x,a,f4.1,3a)')
     #                          '7-point offset: angle ',
     #                           offset_arcmin, ' arcmin ',
     #                          '  aerials ', text

                else if (offset .eq. o_19point) then
                        write (iunit, '(1x,a,f4.1,3a)')
     #                          '19-point offset: angle ',
     #                           offset_arcmin, ' arcmin ',
     #                          '  aerials ', text
                else if (offset .eq. o_37point) then
                        write (iunit, '(1x,a,f4.1,3a)')
     #                          '37-point offset: angle ',
     #                           offset_arcmin, ' arcmin ',
     #                          '  aerials ', text
                else if (offset .eq. o_61point) then
                        write (iunit, '(1x,a,f4.1,3a)')
     #                          '61-point offset: angle ',
     #                           offset_arcmin, ' arcmin ',
     #                          '  aerials ', text

                else if (offset .eq. o_const) then
                        write (iunit, '(1x,a,a,x,f6.2,a,f6.2, a)')
     #                          'fixed offset, aerials ',text,
     #                          offset_W, ' arcmin W, ',
     #                          offset_N, ' arcmin N'

                else if (offset .eq. o_scan)  then
                        write (iunit, '(1x, a,a)')
     #                          'scanned offset, aerials ',text

                else if (offset .eq. o_hex)   then
                        write (iunit, '(1x,a,a)')
     #                          'hexagonal raster, aerials ',text
                        write (iunit,
     #                        '(1x,a,i3,a,i3,a,f5.1,a,a,i3,a,i3,a)')
     #                          'grid ',2*offset2Nx+1,
     #                          ' x ',  2*offset2Ny,
     #                          '  step ', offset_arcmin, ' arcmin;',
     #                          ' 1st row: ',offset_row,
     #                          '   dwell: ',offset_time, ' samples'

                else if (offset .eq. o_m5point) then
                        write (iunit, '(1x,a,f4.1,3a)')
     #                          '5-point offset (all centres): angle ',
     #                           offset_arcmin, ' arcmin ',
     #                          '  aerials ', text


                endif
         endif


C   Print observation start and stop times
C
         IPRINT=1
         LTIME='UT'
         write(iunit,*)
         IF (IUTIM.EQ.1) LTIME='BST'
         IDATE(1)=IDAT1(1)
         IDATE(2)=IDAT1(2)
         IDATE(3)=IDAT1(3)
         CALL CHR_CHDATE(IDATE,IPRINT,CDATE,LD)
         write(STRING,6)ISTIM1(3),ISTIM1(2),ISTIM1(1),
     :     ITIM1(3),ITIM1(2),LTIME,CDATE
         write(iunit,'(2X,A)')STRING
         IDATE(1)=IDAT2(1)
         IDATE(2)=IDAT2(2)
         IDATE(3)=IDAT2(3)
         CALL CHR_CHDATE(IDATE,IPRINT,CDATE,LD)
         write(STRING,6)ISTIM2(3),ISTIM2(2),ISTIM2(1),
     :     ITIM2(3),ITIM2(2),LTIME,CDATE
         STRING(1:11)='Stop time .'
         write(iunit,'(2X,A)')STRING
    6    FORMAT('Start time ',11('.'),2X,2(I2.2,'.'),I2.2,' ST',
     :     2X,I2.2,'.',I2.2,X,A,2X,A)
C
C   Print comment lines, if any
C
         DO I=1,5
           LC=CHR_LENB(COMMENT_LINES(I)(1:76))
           IF (LC.GT.0) THEN
             IF (I.EQ.1) write(iunit,*)
             write(iunit,'(2X,A)')COMMENT_LINES(I)(1:LC)
           ENDIF
         ENDDO
C
       ENDIF
C
       IF (OPTION.EQ.'PARAMETERS') write(iunit,*)
       write(iunit,*)
C
       END
