
C+next_sample_file

        subroutine next_sample_file (file_name, s)

* attempts to find the next sample file in a series of
* multi-centre RT observations

* given

        character*(*)   file_name
        integer         s

* returns the new file name in the same variable
* and s=0; else file_name unchanged and s=-1
* this version cycles round the list

* 18 May 2004; 14 Apr 2005  GP
C-

       include  '/mrao/post/include/control_tables.inc'
       include  '/mrao/post/include/control_tab_v2.inc'

       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'




        character*80    old_file, name

        integer         lun, i, j, k, unit, m
*       integer         blocksize
*
        if (s .ne. 0) return
        old_file = file_name

*        write (*,*) 'next_s_f called: ', file_name

        call open_sf (lun, old_file, 'READ', 0, s)

        if (s .ne. 0) goto 9000
*        blocksize = (page*4)
*        call io_operan(lun, old_file, 'READ', blocksize, 0, s)
*
*        if (s .eq. 0) then
          call read_ct(lun, s)
*        end if
        call close_sf(lun, s)
        if (s .ne. 0) goto 9000

        j = chr_ilstc(old_file,      '/') - 1
        k = chr_ilstc(old_file(1:j), '/') + 1

        name = old_file(k:j)

*        write (*, '(x, a, 2i4)') 'next_sf: ', j, k
*        write (*,'(x, a,a)') 'next_s_f: ', name

        if (Ncentre .eq. 1) then
            s = -1
            goto 9000
        else
            s = -1
            do i = 1, Ncentre
                if (name(1:chr_lenb(name))
     *    .eq. file_list(i)(1:index(file_list(i), '/')-1)) then
                  j = i+1
                  if (i .eq. Ncentre) j = 1
                  file_name =
     &              file_list(j)(1:index(file_list(j),'/')-1)
                  s = 0
                  call io_opefil (unit, '/tmp/RT-sample-file.list',
     *                                  'WRITE', 0, s)
                  if (s .ne. 0) goto 9000
                  m = index (file_name, '-')
                  write (unit, '(a)') file_name
                  write (unit, '(a)') file_name(1:m-1)
                  write (unit, '(a)') file_name(m+1:)
                  close (unit)
                  return
                endif
            enddo
        endif
        if (s .eq. 0) return
 9000   call io_wrerr(s, ' in next_sample_file')
        return
        end



