C POSTMORTEM plot control
C -----------------------
C
C Record
       integer      plot_record(20)
C Number of segments of plot:
       integer      segment_x, segment_y
       equivalence (segment_x, plot_record(1))
       equivalence (segment_y, plot_record(2))
C Plot type
       integer      plot_mode
       equivalence (plot_mode, plot_record(3))
C Current Page in multiple plot
       integer      plot_page
       equivalence (plot_page, plot_record(4))
C Number of pages making a full page
       integer      full_page
       equivalence (full_page, plot_record(5))
C Type of scaling in Y (0=auto-scale, 1=fixed-scale)
       integer      plot_scale_type
       real*4       plot_limit_min, plot_limit_max
       equivalence (plot_scale_type, plot_record(6))
       equivalence (plot_limit_min, plot_record(7))
       equivalence (plot_limit_max, plot_record(8))
C Prompting status
       integer      plot_prompt
       equivalence (plot_prompt, plot_record(9))
C Phase plot type (=1 for plot of phase)
       integer      plot_phase_type
       equivalence (plot_phase_type, plot_record(10))
C Scale of Phase plot
       real*4       plot_phi_min, plot_phi_max
       equivalence (plot_phi_min, plot_record(11))
       equivalence (plot_phi_max, plot_record(12))
C Data type of plot Amp-phase or Cos-Sin
       integer      plot_data_type
       equivalence (plot_data_type, plot_record(13))
C Flags indicating which plot is in progress
       logical      plot_upper, plot_lower
       equivalence (plot_upper, plot_record(14))
       equivalence (plot_lower, plot_record(15))
C Flag indicating plotting of fits to phase data
       logical      plot_fit
       equivalence (plot_fit,   plot_record(16))
C Flag indicating plotting of zero data values (otherwise ignored)
       logical      plot_zeros
       equivalence (plot_zeros, plot_record(17))
C
C Place this in common
       common /post_plot_control/  plot_record
C
C
C Define constants
C    Plot modes
       integer    plot_normal
       parameter (plot_normal     = 0)
       integer    plot_multiple
       parameter (plot_multiple   = 1)
       integer    plot_brief
       parameter (plot_brief      = -1)
       integer    plot_scaled_phi
       parameter (plot_scaled_phi = -2)
       integer    plot_Ampphi
       parameter (plot_Ampphi     = 2)
       integer    plot_cossin
       parameter (plot_cossin     = 1)
C
C
