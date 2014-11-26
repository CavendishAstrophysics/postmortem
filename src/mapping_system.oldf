C
C+mapping_system
C
      subroutine mapping_system(  def_sf, def_lsf, s )
C
C     The mapping system within postmortem.
C
C     Given:
C         Current sample file name
              character*(*)       def_sf
C         Current logical sample file key
              integer             def_lsf

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Combines all the mapping system functions in one place.
C
C-
C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/post/include/mapping_commands.inc'
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/post/include/post_common.inc'
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/include/iolib_errors.inc'

C     Variables, equivalences and commons
C         Current command.
              integer         command
C         Fortran unit number of current map.
              integer         lun
C         Current default map directory
              character*32    map_dir
C         General purpose file name.
              character*64    file_name
C         New lsf key
              integer         new_lsf
C         Dummy variables used for batch submission
c             integer         i4_values(20)
c             real*4          r4_values(20)
c             real*8          r8_values(20)
C         Command line prompt and length.
              character*40    prompt
              integer         lp
              data            prompt, lp / 'Map> ', 5 /
C         General purpose string, indexes and length
              character*80    string
              integer         i1, i2
              integer         ls
C         Current redtape save buffer
              integer         buffer(512)
C         Environment variables returned from io_enqexe
C         Current and previous output device unit number.
              integer         out, old_out

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

      call enmdir( map_dir, s )
      i2 = chr_ilstc( def_sf, '/') - 1
      i1 = chr_ilstc( def_sf(1:i2), '/') + 1
      call io_makfil( map_dir, def_sf(i1:i2), 'map', def_map, ls )
      call io_wrout( ' ' )
      string = 'Initialising map for ' // def_sf
      call io_wrout( string )
      call map_init_redtape( def_sf, def_lsf, s )
      call io_wrout( ' ' )

C     ****************************************************************
C
C         Main Code
C         ---------
C

  100 continue
          call cmd_getcmd(prompt(1:lp), cmd_list, num_cmds, command, s)

          if (s .ne. 0) then
              continue
          else if (command .le. 0) then
              call basic_commands( command, prompt, lp, s )
          else if ( command .eq. set_sample_file_cmd ) then
              file_name  = ' '
              new_lsf = -1
              call map_init_redtape( file_name, new_lsf, s )
              if ( s .eq. 0 ) then
                  def_sf  = file_name
                  def_lsf = new_lsf
                  call io_wrout ( ' ' )
                  call io_wrout ( def_sf )
                  call io_wrout ( ' ' )
                  i2 = chr_ilstc( def_sf, '/') - 1
                  i1 = chr_ilstc( def_sf(1:i2), '/') + 1
                  call io_makfil( map_dir, def_sf(i1:i2), 'map',
     *                                                     def_map, ls )
              end if
          else if (command .eq. set_lsf_cmd) then
              new_lsf = 0
              call map_init_redtape( def_sf, new_lsf, s )
              if ( s .eq. 0 ) then
                  def_lsf = new_lsf
              else if ( s .eq. NO_LSFSAVED ) then
                  s = 0
                  call io_wrout( 'No logical sample files are saved.' )
              end if
          else if ( command .eq. open_map_cmd ) then
              file_name = ' '
              call dpredt( buffer, s )
              call getmap( 'Map name : ', def_map, file_name, s )
              if ( s .eq. 0 ) then
                call opemap( lun, file_name, 'READ', 0, s )
                call rdredt( lun, 0, s )
                call map_chredt( s )
                close ( lun )
              end if

              if ( s .eq. 0 ) then
                call io_wrout( ' ' )
                def_map = file_name
                call enmlsf( 1, file_name, new_lsf, s )
                call enq_sfname( file_name, file_name, .false., s )
                if (s.ne.0) then
                  s = 0
                  call io_wrout('The LSF for this map is not available')
                else
                  call io_wrout( def_map )
                  def_sf  = file_name
                  def_lsf = new_lsf
                end if
              else if ( s .ne. usr_break ) then
                s = 0
               call io_wrout('That file is not a properly defined map.')
                call ldredt( buffer, s )
              end if
              call io_wrout( ' ' )
          else if ( command .eq. save_map_cmd ) then
              call map_save( def_map, s )
          else if ( command .eq. select_conv_cmd ) then
              call map_sel_conv( s )
          else if ( command .eq. select_grading_cmd ) then
              call map_sel_grading( s )
          else if ( command .eq. select_weight_cmd ) then
              call map_sel_weight( s )
          else if ( command .eq. select_size_cmd ) then
              call map_sel_size( s )
          else if ( command .eq. select_type_cmd ) then
              call map_sel_type( def_map, s )
          else if ( command .eq. select_proj_cmd ) then
              call map_sel_proj( s )
          else if ( command .eq. select_gr_cent_cmd ) then
              call map_sel_gr_cent( s )
          else if ( command .eq. select_uv_cent_cmd ) then
              call map_sel_uv_cent( s )
          else if ( command .eq. add_lsf_cmd ) then
              call map_add_lsf( s )
          else if ( command .eq. delete_lsf_cmd ) then
              call map_delete_lsf( s )
          else if ( command .eq. plot_cmd ) then
              call map_plot( plot_device, s )
          else if ( command .eq. print_redtape_cmd ) then
              call io_enqout( old_out )
              call io_opeout( out, s )
              call exm_print( 'ALL', s )
              if ( out .ne. old_out ) then
                  call io_close( out, s )
                  call io_setout( old_out )
              end if
          else if ( command .eq. make_map_cmd ) then
              file_name = ' '
              call io_system( 'setenv SAMPDIR '//def_dir, s )
              call getmap( 'Map name : ', def_map, file_name, s )
              call io_system( '/mrao/bin/mapper '//file_name//' &', s )
              if ( s .eq. 0 ) then
                  def_map = file_name
                  call io_wrout( ' ' )
                  call io_wrout( def_map )
                  call io_wrout( ' ' )
              end if
          else
              ls = chr_lend( cmd_list(command), '.' )
              call io_wrout(
     *          'Command '//cmd_list(command)(1:ls)//'not implemented.')
          end if

C         Restore status.
          if ( s .eq. usr_break ) s = 0
          if ( s .ne. 0 ) goto 9999
      if ( command .ne. exit_cmd ) goto 100

      call io_enqcli( string, ls )
      if (ls.eq.0) call io_wrout( ' ' )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call io_wrerr( s, 'in subroutine MAPPING_SYSTEM' )
          return
      end
