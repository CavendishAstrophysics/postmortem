C PostMortem Global Variables
C ---------------------------
C
C DJT            19/08/93  Unix version
C
C This file contains global variables for the POSTORTEM program
C

C Current default sample file directory
       character*32  def_dir

C Current default map name
       character*64  def_map

C Current PGPLOT plot device
       character*20  plot_device

C Initialisation flags for each subsystem
       logical cal_init, ion_init, map_init, rem_init

       common /post_common/ def_dir, def_map, plot_device
       common /post_control/ cal_init, ion_init, map_init, rem_init

