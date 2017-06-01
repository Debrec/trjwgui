!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Convertir de niveles de presion a temperatura potencial y biceversa
!----------------------------------------------------------------------
!----------------------------------------------------------------------
MODULE ptotheta
	USE interpolacion
	IMPLICIT none

  PRIVATE
  REAL, PARAMETER :: ktheta=0.286
  REAL, PARAMETER :: P0=1013 ! Surface presure (hPa)
  INTEGER :: ints ! Interpolation 0 for lineal 1 for splines
  PUBLIC :: p2theta, theta2p, ctheta

CONTAINS
  REAL FUNCTION ctheta(P,T,Ps)
    REAL, INTENT(IN) :: P,T
    REAL, INTENT(IN), OPTIONAL :: Ps
		REAL :: Psurf
    IF(PRESENT(Ps)) THEN
       Psurf=Ps ! If the optional argument Ps
    ELSE         ! exist, use this as the surface
       Psurf=P0 ! presure for the pot. temp. calc.
    END IF

    ctheta=T*(Psurf/P)**(ktheta)

  END FUNCTION ctheta

  REAL FUNCTION p2theta(P,T,theta,var,nz,Ps)
    INTEGER, INTENT(IN) :: nz
    REAL, INTENT(IN) ::  P(nz),T(nz),var(nz) ! Input profiles
    REAL, INTENT(IN) ::theta ! theta level
    REAL, INTENT(IN), OPTIONAL :: Ps

    REAL :: thetap(SIZE(P))
    REAL :: Psurf, P_th, T_th, theta_th
    INTEGER :: npl, i_th, jpl
    LOGICAL :: inc

    npl=SIZE(P)

    IF(PRESENT(Ps)) THEN
        Psurf=Ps ! If the optional argument Ps
    ELSE         ! exist, use this as the surface
        Psurf=P0 ! presure for the pot. temp. calc.
    END IF

    ints=0 ! lineal interpolation

    !Calule the theta profiles
    DO Jpl=1,npl
       thetap(Jpl)=T(Jpl)*(Psurf/P(Jpl))**(ktheta)
    END DO

    !And check the profiles order
    IF(P(1).GT.P(npl)) THEN
       inc=.TRUE.
    ELSE IF(P(1).LT.P(npl)) THEN
       inc=.FALSE.
    ELSE
       WRITE(0,*) 'Wrong profile data'
       STOP
    END IF

    IF(ints.EQ.0) THEN ! lineal interpolation
    	!Now look for corresponding theta level
    	IF(inc.EQV..TRUE.) THEN
       	DO Jpl=1,npl-1
          	IF((theta.GE.thetap(Jpl)).AND.(theta.LT.thetap(Jpl+1))) THEN
             	i_th=Jpl
          	END IF
       	END DO
    	ELSE
       	DO Jpl=1,npl-1
          	IF((theta.LT.thetap(Jpl)).AND.(theta.GE.thetap(Jpl+1))) THEN
             	i_th=Jpl
          	END IF
       	END DO
    	END IF

    	! Do the maths
    	P_th=intp1d(thetap(i_th),thetap(i_th+1),theta,P(i_th:i_th+1))
    	T_th=intp1d(thetap(i_th),thetap(i_th+1),theta,T(i_th:i_th+1))
    	p2theta=intp1d(thetap(i_th),thetap(i_th+1),theta,var(i_th:i_th+1))
    ELSE

       WRITE(0,*)'Unknown interpolation method'
    END IF

    !Check for consistency
    theta_th=T_th*(Psurf/P_th)**(ktheta)
    IF((theta_th.GT.(theta*1.05)).OR.(theta_th.LT.(theta*0.95))) THEN
      WRITE(*,*) 'New theta show diferences greater than 5% with previous theta'
      WRITE(*,*) theta_th, theta
    ELSE
    END IF

  END FUNCTION p2theta

  REAL FUNCTION theta2p(theta,T,P,var,Ps)
    REAL, INTENT(IN) ::  theta(:),T(:),var(:) ! Input profiles
    REAL, INTENT(IN) :: P ! theta level
    REAL, INTENT(IN), OPTIONAL :: Ps

    REAL :: ptheta(SIZE(theta))
    REAL :: Psurf, P_th, T_th, theta_th
    INTEGER :: npl, i_th, jpl
    LOGICAL :: inc

    npl=SIZE(theta)

    IF(PRESENT(Ps)) THEN
        Psurf=Ps ! If the optional argument Ps
    ELSE         ! exist, use this as the surface
        Psurf=P0 ! presure for the pot. temp. calc.
    END IF

    !Calule the theta profiles
    DO Jpl=1,npl
       ptheta(Jpl)=Psurf*(T(Jpl)/theta(Jpl))**(1/ktheta)
    END DO

    ints=0 ! lineal interpolation

    !And check the profiles order
    IF(theta(1).GT.theta(npl)) THEN
       inc=.TRUE.
    ELSE IF(theta(1).LT.theta(npl)) THEN
       inc=.FALSE.
    ELSE
       WRITE(0,*) 'Wrong profile data'
       STOP
    END IF

    IF(ints.EQ.0) THEN ! lineal interpolation
    !Now look for corresponding theta level
    	IF(inc.EQV..TRUE.) THEN
       	DO Jpl=1,npl-1
          	IF((P.GE.ptheta(Jpl)).AND.(P.LT.ptheta(Jpl+1))) THEN
             	i_th=Jpl
          	END IF
       	END DO
    	ELSE
       	DO Jpl=1,npl-1
          IF((P.LT.ptheta(Jpl)).AND.(P.GE.ptheta(Jpl+1))) THEN
             i_th=Jpl
          END IF
       	END DO
    	END IF

    	! Do the maths
    	theta_th=intp1d(ptheta(i_th),ptheta(i_th+1),P,theta(i_th:i_th+1))
    	T_th=intp1d(ptheta(i_th),ptheta(i_th+1),P,T(i_th:i_th+1))
    	theta2p=intp1d(ptheta(i_th),ptheta(i_th+1),P,var(i_th:i_th+1))
    ELSE
       WRITE(0,*)'Unknown interpolation method'
    END IF
    !Check for consistency
    P_th=Psurf*(T_th/theta_th)**(1/ktheta)
    IF((P_th.GT.(P*1.05)).OR.(P_th.LT.(P*0.95))) THEN
       WRITE(*,*) 'New theta show diferences greater than 5% with previous P'
       WRITE(*,*) P_th, P
    END IF

  END FUNCTION theta2p

END MODULE ptotheta
