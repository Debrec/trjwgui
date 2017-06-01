!----------------------------------------------------------------------
!----------------------------------------------------------------------
! and currently experimental 3D interface.
! Stereographic to geographic translation code
!----------------------------------------------------------------------
!----------------------------------------------------------------------
       SUBROUTINE ang_to_str(rlong,rlat,xstr,ystr)
         USE parametros
	 IMPLICIT none
	 REAL, INTENT(IN) :: rlong, rlat
         REAL, INTENT(OUT) :: xstr, ystr
 	 REAL M
         M=1

	 xstr=M*tan(Pi/4+rlat/2)*sin(rlong);
         ystr=-M*tan(Pi/4+rlat/2)*cos(rlong);

       END SUBROUTINE ang_to_str

       SUBROUTINE str_to_ang(rlong,rlat,xstr,ystr)
         USE parametros
	 IMPLICIT none
	 REAL, INTENT(OUT) :: rlong, rlat
         REAL, INTENT(IN) :: xstr, ystr
         REAL M
         M=1
         if((xstr.GE.0).AND.(ystr.LT.0))rlong=ATAN(-xstr/ystr);
         if((xstr.GT.0).AND.(ystr.GT.0))rlong=Pi+ATAN(-xstr/ystr);
         if((xstr.LT.0).AND.(ystr.GT.0))rlong=Pi+ATAN(-xstr/ystr);
         if((xstr.LT.0).AND.(ystr.LT.0))rlong=2.*Pi+ATAN(-xstr/ystr);
         if(ystr.EQ.0)rlong=0
	 rlat=2.*ATAN(sqrt(xstr*xstr+ystr*ystr)/M)-Pi/2.;
       END SUBROUTINE str_to_ang

       SUBROUTINE strwinds(rlong,rlat,U,V,Ustr,Vstr)
        USE parametros
        IMPLICIT none
	REAL,INTENT(IN) :: rlong,rlat,U,V
	REAL, INTENT(OUT) :: Ustr,Vstr
        REAL M
	M=1

	Ustr=M*V*sin(rlong)/(2*Rt*cos(Pi/4+rlat/2)*cos(Pi/4+rlat/2))&
             +M*U*cos(rlong)*sqrt(2.)/(2*Rt*cos(Pi/4+rlat/2)*&
             (cos(rlat/2)-sin(rlat/2)))

	Vstr=-M*V*cos(rlong)/(2*Rt*cos(Pi/4+rlat/2)*cos(Pi/4+rlat/2))&
             +M*U*sin(rlong)*sqrt(2.)/(2*Rt*cos(Pi/4+rlat/2)*&
             (cos(rlat/2)-sin(rlat/2)))

       END SUBROUTINE strwinds

       SUBROUTINE ang_to_strN(rlong,rlat,xstr,ystr)
         USE parametros
	 IMPLICIT none
	 REAL, INTENT(IN) :: rlong, rlat
         REAL, INTENT(OUT) :: xstr, ystr
 	 REAL M
         M=1

	 xstr=M*tan(Pi/4-rlat/2)*sin(rlong);
         ystr=-M*tan(Pi/4-rlat/2)*cos(rlong);

       END SUBROUTINE ang_to_strN

       SUBROUTINE str_to_angN(rlong,rlat,xstr,ystr)
         USE parametros
	 IMPLICIT none
	 REAL, INTENT(OUT) :: rlong, rlat
         REAL, INTENT(IN) :: xstr, ystr
         REAL M
         M=1
         if((xstr.GE.0).AND.(ystr.LT.0))rlong=ATAN(-xstr/ystr);
         if((xstr.GT.0).AND.(ystr.GT.0))rlong=Pi+ATAN(-xstr/ystr);
         if((xstr.LT.0).AND.(ystr.GT.0))rlong=Pi+ATAN(-xstr/ystr);
         if((xstr.LT.0).AND.(ystr.LT.0))rlong=2.*Pi+ATAN(-xstr/ystr);
         if(ystr.EQ.0)rlong=0
	 rlat=-2.*ATAN(sqrt(xstr*xstr+ystr*ystr)/M)+Pi/2.;
       END SUBROUTINE str_to_angN

       SUBROUTINE strwindsN(rlong,rlat,U,V,Ustr,Vstr)
        USE parametros
        IMPLICIT none
	REAL,INTENT(IN) :: rlong,rlat,U,V
	REAL, INTENT(OUT) :: Ustr,Vstr
        REAL M
	M=1

	Ustr=-M*V*sin(rlong)/(2*Rt*cos(Pi/4-rlat/2)*cos(Pi/4-rlat/2))&
             +M*U*cos(rlong)*sqrt(2.)/(2*Rt*cos(Pi/4-rlat/2)*&
             (cos(rlat/2)+sin(rlat/2)))

	Vstr=+M*V*cos(rlong)/(2*Rt*cos(Pi/4-rlat/2)*cos(Pi/4-rlat/2))&
             +M*U*sin(rlong)*sqrt(2.)/(2*Rt*cos(Pi/4-rlat/2)*&
             (cos(rlat/2)+sin(rlat/2)))

       END SUBROUTINE strwindsN
