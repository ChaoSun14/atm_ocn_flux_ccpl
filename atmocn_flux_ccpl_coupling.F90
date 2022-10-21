! Edited by Sun Chao at 2022-07 
! 

module atmocn_flux_ccpl_coupling_mod

    use CCPL_interface_mod
    use atmocn_flux_ccpl_kinds
    use shr_flux_mod,      only: shr_flux_atmocn

    implicit none  

contains

    subroutine atmocn_flux_ccpl_coupling_registration

        implicit none

        integer :: local_size(1), ierror

        call mpi_comm_size(atmocn_flux_ccpl_comm,ccpl_npe_all,ierror)
        call mpi_comm_rank(atmocn_flux_ccpl_comm,ccpl_mype_all,ierror)

        atmocn_flux_grid_id = CCPL_external_modules_para_get_field_grid_ID(atmocn_flux_instance_id,"So_tref",annotation="get grid id")
        atmocn_flux_decomp_id = CCPL_external_modules_para_get_field_decomp_ID(atmocn_flux_instance_id,"So_tref",annotation="get grid decomp id")

        ccpl_dead_comps = CCPL_external_modules_para_get_control_var(atmocn_flux_instance_id, 1, annotation="if dead components are used, 1 for true, 0 for false")
        ccpl_nloc = CCPL_external_modules_para_get_control_var(atmocn_flux_instance_id, 2, annotation="get the number of local grid cells")

        write(*,*) "[CCPL <atmocn_flux>] number of local grid cells: ", ccpl_nloc
        local_size(1) = ccpl_nloc

        field_id_xao_So_tref=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_So_tref,"So_tref",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Reference temperature at 2 meters")
        field_id_xao_So_qref=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_So_qref,"So_qref",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Reference specific humidity at 2 meters")
        field_id_xao_So_ustar=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_So_ustar,"So_ustar",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Surface fraction velocity in ocean")
        field_id_xao_So_re=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_So_re,"So_re",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Square of exch. coeff (tracers)")
        field_id_xao_So_ssq=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_So_ssq,"So_ssq",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Surface saturation specific humidity in ocean")
        field_id_xao_So_u10=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_So_u10,"So_u10",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "10m wind")
        field_id_xao_So_duu10n=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_So_duu10n,"So_duu10n",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Wind speed squared at 10 meters")
        field_id_xao_Faox_taux=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_Faox_taux,"Faox_taux",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Zonal surface stress")
        field_id_xao_Faox_tauy=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_Faox_tauy,"Faox_tauy",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Meridional surface stress")
        field_id_xao_Faox_lat=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_Faox_lat,"Faox_lat",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Surface latent heat flux")
        field_id_xao_Faox_sen=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_Faox_sen,"Faox_sen",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Sensible heat flux")
        field_id_xao_Faox_evap=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_Faox_evap,"Faox_evap",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Evaporation water flux")
        field_id_xao_Faox_lwup=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_xao_Faox_lwup,"Faox_lwup",CCPL_PARA_TYPE_OUT, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Surface upward longwave heat flux")
        field_id_a2x_Sa_z=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_a2x_Sa_z,"Sa_z",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Height at the lowest model level")
        field_id_a2x_Sa_u=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_a2x_Sa_u,"Sa_u",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Zonal wind at the lowest model level")
        field_id_a2x_Sa_v=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_a2x_Sa_v,"Sa_v",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Meridional wind at the lowest model level")
        field_id_a2x_Sa_tbot=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_a2x_Sa_tbot,"Sa_tbot",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Temperature at the lowest model level")
        field_id_a2x_Sa_ptem=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_a2x_Sa_ptem,"Sa_ptem",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Potential temperature at the lowest model level")
        field_id_a2x_Sa_shum=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_a2x_Sa_shum,"Sa_shum",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Specific humidity at the lowest model level")
        field_id_a2x_Sa_dens=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_a2x_Sa_dens,"Sa_dens",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Density at the lowest model level")
        field_id_o2x_So_t=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_o2x_So_t,"So_t",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Surface temperature")
        field_id_o2x_So_u=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_o2x_So_u,"So_u",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Zonal sea water velocity")
        field_id_o2x_So_v=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_o2x_So_v,"So_v",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Meridional sea water velocity")
        field_id_mask=CCPL_external_modules_declare_argument(atmocn_flux_instance_id,ccpl_mask,"mask_ccpl",CCPL_PARA_TYPE_IN, atmocn_flux_decomp_id, &
        &                                                              atmocn_flux_grid_id, local_size, "Global grid cell mask")

    end subroutine atmocn_flux_ccpl_coupling_registration

   subroutine ccpl_atmocn_flux_run

    implicit none
    integer     :: n
    logical     :: dead_comps   ! .true.  => dead components are used

    if(.not.allocated(emask)) allocate(emask(ccpl_nloc))
    if(.not.allocated(uocn)) allocate(uocn(ccpl_nloc)) 
    if(.not.allocated(vocn)) allocate(vocn(ccpl_nloc)) 
    if(.not.allocated(tocn)) allocate(tocn(ccpl_nloc)) 
    if(.not.allocated(zbot)) allocate(zbot(ccpl_nloc)) 
    if(.not.allocated(ubot)) allocate(ubot(ccpl_nloc)) 
    if(.not.allocated(vbot)) allocate(vbot(ccpl_nloc)) 
    if(.not.allocated(thbot)) allocate(thbot(ccpl_nloc))
    if(.not.allocated(shum)) allocate(shum(ccpl_nloc)) 
    if(.not.allocated(dens)) allocate(dens(ccpl_nloc)) 
    if(.not.allocated(tbot)) allocate(tbot(ccpl_nloc)) 
    if(.not.allocated(sen)) allocate(sen(ccpl_nloc)) 
    if(.not.allocated(lat)) allocate(lat(ccpl_nloc)) 
    if(.not.allocated(lwup)) allocate(lwup(ccpl_nloc)) 
    if(.not.allocated(evap)) allocate(evap(ccpl_nloc)) 
    if(.not.allocated(taux)) allocate(taux(ccpl_nloc)) 
    if(.not.allocated(tauy)) allocate(tauy(ccpl_nloc)) 
    if(.not.allocated(tref)) allocate(tref(ccpl_nloc)) 
    if(.not.allocated(qref)) allocate(qref(ccpl_nloc)) 
    if(.not.allocated(duu10n)) allocate(duu10n(ccpl_nloc))
    if(.not.allocated(ustar)) allocate(ustar(ccpl_nloc))
    if(.not.allocated(re)) allocate(re(ccpl_nloc))   
    if(.not.allocated(ssq)) allocate(ssq(ccpl_nloc)) 
    emask = ccpl_mask
    dead_comps = .false.
    if (ccpl_dead_comps.eq.1) dead_comps = .true.
    if (dead_comps) then
       do n = 1,ccpl_nloc
          ccpl_mask(n) =   1      ! ocn domain mask            ~ 0 <=> inactive cell
          tocn(n) = 290.0_R8 ! ocn temperature            ~ Kelvin
          uocn(n) =   0.0_R8 ! ocn velocity, zonal        ~ m/s
          vocn(n) =   0.0_R8 ! ocn velocity, meridional   ~ m/s
          zbot(n) =  55.0_R8 ! atm height of bottom layer ~ m
          ubot(n) =   0.0_R8 ! atm velocity, zonal        ~ m/s
          vbot(n) =   2.0_R8 ! atm velocity, meridional   ~ m/s
          thbot(n)= 301.0_R8 ! atm potential temperature  ~ Kelvin
          shum(n) = 1.e-2_R8 ! atm specific humidity      ~ kg/kg
          dens(n) =   1.0_R8 ! atm density                ~ kg/m^3
          tbot(n) = 300.0_R8 ! atm temperature            ~ Kelvin
       enddo
    else    
       do n = 1,ccpl_nloc
          if (ccpl_mask(n) /= 0) then    
             zbot(n) = ccpl_a2x_Sa_z(n)
             ubot(n) = ccpl_a2x_Sa_u(n)
             vbot(n) = ccpl_a2x_Sa_v(n)
             thbot(n)= ccpl_a2x_Sa_ptem(n)
             shum(n) = ccpl_a2x_Sa_shum(n)
             dens(n) = ccpl_a2x_Sa_dens(n)
             tbot(n) = ccpl_a2x_Sa_tbot(n)
             tocn(n) = ccpl_o2x_So_t(n)   
             uocn(n) = ccpl_o2x_So_u(n)
             vocn(n) = ccpl_o2x_So_v(n)
             !--- mask missing atm or ocn data
             if (dens(n) < 1.0e-12 .or. tocn(n) < 1.0) then
                emask(n) = 0
                !write(logunit,*) 'aoflux tcx1',n,dens(n),tocn(n)
             endif
          end if
       enddo
    end if

    call shr_flux_atmocn (ccpl_nloc , zbot , ubot, vbot, thbot, &
                          shum , dens , tbot, uocn, vocn , &
                          tocn , emask, sen , lat , lwup , &
                          evap , taux , tauy, tref, qref , &
! missval should not be needed if flux calc consistent with mrgx2a fraction
!                         duu10n,ustar, re  , ssq, missval = 0.0_r8 )
                          duu10n,ustar, re  , ssq)

    do n = 1,ccpl_nloc
       if (ccpl_mask(n) /= 0) then   
          ccpl_xao_Faox_sen(n)  = sen(n)
          ccpl_xao_Faox_lat(n)  = lat(n)
          ccpl_xao_Faox_taux(n) = taux(n)
          ccpl_xao_Faox_tauy(n) = tauy(n)
          ccpl_xao_Faox_evap(n) = evap(n)
          ccpl_xao_So_tref(n)   = tref(n)
          ccpl_xao_So_qref(n)   = qref(n)
          ccpl_xao_So_ustar(n)  = ustar(n)  ! friction velocity
          ccpl_xao_So_re(n)     = re(n)     ! reynolds number
          ccpl_xao_So_ssq(n)    = ssq(n)    ! s.hum. saturation at Ts
          ccpl_xao_Faox_lwup(n) = lwup(n)   
          ccpl_xao_So_duu10n(n) = duu10n(n)  
          ccpl_xao_So_u10(n)    = sqrt(duu10n(n))  
       end if
    enddo


   end subroutine ccpl_atmocn_flux_run

end module atmocn_flux_ccpl_coupling_mod
