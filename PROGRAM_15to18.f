************************************************************************
*       CIRCULATION AND SEDIMENTATION IN A 2-D TURBULENT PLANE JET     *
*                                                                      *
************************************************************************
*       This driver program solves the equations describing horizontal *
*       velocities in a buoyant, turbulent, plane jet issuing in a     *
*       normal direction from a coast into a large volume of still     *
*       fluid.  Sedimentation under the jet is modelled using a        *
*       hemipelagic rain formulation, bedload dumping, and downslope   *
*       diffusion due to slides, slumps and turbidity currents.        *
*       The equations are from Albertson et al. (1950) and             *
*       Syvitski et al. (1988).  Programmed by R. Slingerland, 1991.   *
************************************************************************
      Implicit None
      Integer*2 LI,LJ,i,j,im,jm,n,jj,p,q,jmm,k,km,jmd2,ihold
      Character*80 formt
C-----------------------------------------------------------------------
C  LI and LJ are the dimensions of the grid; N=im*jm
C-----------------------------------------------------------------------
      PARAMETER (LI=20,LJ=20,N=400)
      Real*8 u(LI,LJ),v(LI,LJ),dt,t,DCx,DCy,clevel,
     #ds,xo,Ho,uo,Bo,uorg(LI,LJ),vorg(LI,LJ),tt(LI,LJ),
     #ttorg(LI,LJ),Rorg(LI,LJ),h(LI,LJ),R(LI,LJ),Db,Co(4)
      common/com1/uo,bo,ds,xo,Ho
      common/com2/DCx,DCy,clevel
      common/com3/Co
C-----------------------------------------------------------------------
C    Open the input and output files
C-----------------------------------------------------------------------
      OPEN(10, FILE='input.dat')
      OPEN(12, FILE='U.DATA')
      OPEN(13, FILE='V.DATA')
      OPEN(14, FILE='R.DATA')
      OPEN(15, FILE='h.DATA')
C-----------------------------------------------------------------------
C  Read the Input Variables
C-----------------------------------------------------------------------
      READ (10, 102) IM, JM, KM
      READ (10, 100) DT
      READ (10, 100) DS
      READ (10, 100) UO
      READ (10, 101) BO, HO
      READ (10, 101) DCX, DCY
      READ (10, 100) CLEVEL
      READ (10, 100) DB
      READ (10, 101) CO(1), CO(2)
      READ (10, 101) CO(3), CO(4)
  100   format(65x,f10.3)
  101   format(55x,f10.3,3x,f10.3)
  102   format(/60x,I5,2x,I5,2x,I5)
C-----------------------------------------------------------------------
C  Calculate the x- and y-directed flow velocities
C-----------------------------------------------------------------------
      CALL VEL (U, V, IM, JM)
C-----------------------------------------------------------------------
C  Calculate the transit time for a parcel of fluid
C-----------------------------------------------------------------------
      CALL TRANSTIME (U, V, IM, JM, TT)
C-----------------------------------------------------------------------
C  Calculate the hemipelagic rain
C-----------------------------------------------------------------------
      CALL HEMI (TT, IM, JM, R)
C-----------------------------------------------------------------------
C  Shift values and fill in the mirror image of the flow field
C-----------------------------------------------------------------------
      JMD2 = JM/2
      JMM = JMD2 - 1
      DO I = 1, IM
         DO J = 1, JMD2
            UORG(I,J+JMM) = U(I,J)
            VORG(I,J+JMM) = V(I,J)
            TTORG(I,J+JMM) = TT(I,J)
            RORG(I,J+JMM) = R(I,J)
            JJ = JMD2 + 1 - J
            UORG(I,JJ) = U(I,J)
            VORG(I,JJ) = -V(I,J)
            TTORG(I,JJ) = TT(I,J)
            RORG(I,JJ) = R(I,J)
         END DO
      END DO
      DO I = 1, IM
         DO J = 1, JM
            H(I,J) = 0.0
         END DO
      END DO
      T = 0
C-----------------------------------------------------------------------
C  Start the Main Time Loop
C-----------------------------------------------------------------------
      DO 14 K = 1, KM
         T = T + DT
C-----------------------------------------------------------------------
C  locate the shoreline and shift independent variables accordingly
C-----------------------------------------------------------------------
         IHOLD = 0
         DO I = 1, IM
            IF (H(I,JMD2) .GE. CLEVEL-HO) IHOLD = I
         END DO
         IF (IHOLD .EQ. IM) THEN
            PRINT *, 'basin completely filled; abort run'
            STOP 
         ELSE
            DO I = IM, IHOLD + 1, -1
               DO J = 1, JM
                  U(I,J) = UORG(I-IHOLD,J)
                  V(I,J) = VORG(I-IHOLD,J)
                  R(I,J) = RORG(I-IHOLD,J)
                  TT(I,J) = TTORG(I-IHOLD,J)
               END DO
            END DO
         ENDIF
C------------------------------------------------------------------------
C  If the node is within the region directly in front of the river mouth
C  and less than xo distance into the basin, add bedload; also zero out
C  arrays landward of present shoreline.
C------------------------------------------------------------------------
         DO I = 1, IM
            DO J = 1, JM
               IF (I.GT.IHOLD .AND. I.LT.INT(IHOLD+XO/DS) .AND. J.GT.
     #            JMD2-INT(BO/DS) .AND. J.LT.JMD2+INT(BO/DS)) R(I,J)
     #             = R(I,J) + DB
               IF (I .LE. IHOLD) THEN
                  U(I,J) = 0.0
                  V(I,J) = 0.0
                  R(I,J) = 0.0
               ENDIF
            END DO
         END DO
C-----------------------------------------------------------------------
C  Solve for downslope diffusion due to gravitational processes and
C  calculate new bed elevations due to all processes
C------------------------------------------------------------------------
         CALL BED (IM, JM, T, DT, H, R, DB, IHOLD)
C------------------------------------------------------------------------
C  If the bed elevation reaches the elevation of the river mouth, stop
C  aggradation; note that mass is not conserved by this action
C------------------------------------------------------------------------
         DO I = 1, IM
            DO J = 1, JM
               IF (H(I,J) .GE. CLEVEL-HO) H(I,J) = CLEVEL - HO + 1.0
            END DO
         END DO
C-----------------------------------------------------------------------
C  Write the Results to a Regular File
C------------------------------------------------------------------------
         P = 1
         Q = 7
         FORMT = '(25x,i6)'
         DO WHILE(P .LE. JM)
            Q = MIN0(JM,Q)
            WRITE (12, FORMT) K
            WRITE (13, FORMT) K
            WRITE (14, FORMT) K
            WRITE (15, FORMT) K
            DO I = 1, IM
               WRITE (12, 103) (U(I,J), J = P, Q)
               WRITE (13, 103) (V(I,J), J = P, Q)
               WRITE (14, 103) (R(I,J), J = P, Q)
               WRITE (15, 103) (H(I,J), J = P, Q)
  103 format(4x,7(1x,1e9.2))
            END DO
            P = Q + 1
            Q = Q + 7
         END DO
   14 CONTINUE
      STOP 
      END
************************************************************************
*       This subroutine solves the equations describing the horizontal *
*       velocities in a buoyant, turbulent, plane jet issuing in a     *
*       normal direction from a coast into a large volume of still     *
*       fluid.  The equations are from Albertson et al. (1950) and     *
*       Syvitski et al. (1988).  Programmed by R. Slingerland, 1991.   *
************************************************************************
************************************************************************
      Subroutine vel(u,v,im,jm)
      Implicit None
      Integer*2 i,j,im,jm,jmd2,LI,LJ
      PARAMETER (LI=20,LJ=20)
      Real*8 u(LI,LJ),v(LI,LJ),xo,aa,ubarp,ubarm,
     #pi,ds,uo,Bo,Ho,c1,alpha,beta,y,x
      common/com1/uo,bo,ds,xo,Ho
C-----------------------------------------------------------------------
C  Define certain parameters and Zero Out Velocity Variables
C-----------------------------------------------------------------------
      JMD2 = JM/2
      PI = 3.14159
      C1 = 0.109
      ALPHA = SQRT(1.0/(SQRT(PI)*C1))
      BETA = 1/(2.0*C1**2)
      XO = ALPHA**2*BO
      AA = 0.0966
      DO I = 1, IM
         DO J = 1, JM
            U(I,J) = 0.0
            V(I,J) = 0.0
         END DO
      END DO
      X = 0.0
C-----------------------------------------------------------------------
C  Solve the x and y-directed velocity equations
C-----------------------------------------------------------------------
      DO I = 1, IM
         X = X + DS
         Y = -DS
         DO J = 1, JMD2
            Y = Y + DS
            IF (X .LE. XO) THEN
C-----------------------------------------------------------------------
C  Zone of flow establishment
C-----------------------------------------------------------------------
               IF (Y .GT. 0.5*(BO-X/ALPHA**2)) THEN
                  U(I,J) = UO*EXP((-BETA*(Y+AA*X-BO/2.0)**2/X**2))
               ELSE
                  U(I,J) = UO
               ENDIF
            ELSE
C-----------------------------------------------------------------------
C  Zone of established flow
C-----------------------------------------------------------------------
               U(I,J) = UO*ALPHA*SQRT(BO/X)*EXP((-BETA*Y**2/X**2))
            ENDIF
         END DO
      END DO
C-----------------------------------------------------------------------
C  Solve continuity equation for v's
C-----------------------------------------------------------------------
      DO I = 2, IM - 1
         V(I,1) = 0.0
         V(I,2) = -DS/(2.0*DS)*(U(I+1,1)-U(I-1,1))
         DO J = 2, JMD2 - 1
            UBARP = (U(I+1,J)+U(I+1,J+1))/2.0
            UBARM = (U(I-1,J)+U(I-1,J+1))/2.0
            V(I,J+1) = V(I,J) - DS/(2.0*DS)*(UBARP-UBARM)
         END DO
      END DO
      DO J = 1, JMD2
         V(1,J) = V(2,J)
         V(IM,J) = 2.0*V(IM-1,J) - V(IM-2,J)
      END DO
      RETURN 
      END
************************************************************************
*       This subroutine calculates the transit time for parcels of     *
*       fluid in a 2-D plane jet.                                      *
*       Programmed by R. Slingerland, 1991.                            *
************************************************************************
************************************************************************
      Subroutine transtime(u,v,im,jm,ttime)
      Implicit None
      Integer*2 i,j,im,jm,jj,kcount,jmd2,LI,LJ
      PARAMETER (LI=20,LJ=20)
      Real*8 u(LI,LJ),v(LI,LJ),
     #ds,uo,bo,xo,Ho,y,utemp,vtemp,loc,ttime(LI,LJ)
      common/com1/uo,bo,ds,xo,Ho
      INTEGER KCOU1X
C-----------------------------------------------------------------------
C  Define certain parameters and zero out arrays
C-----------------------------------------------------------------------
      DO I = 1, IM
         DO J = 1, JM
            TTIME(I,J) = 0.0
         END DO
      END DO
      JMD2 = JM/2
C-----------------------------------------------------------------------
C  Calculate travel times for particles by projecting particle paths
C  back to the river mouth. Accomplish this as follows:
C  Calculate linear average u and v velocities at x,y; find intersection
C  of particle path with i-1; calculate travel time between i and
C  i-1; sum over all i. A travel time of zero means that no parcels
C  of river water pass that node.
C-----------------------------------------------------------------------
      DO I = 1, IM
         DO J = 2, JMD2
            KCOUNT = I
            IF (U(I,J) .GT. 0.001D0) THEN
               Y = (J-1)*DS
               DO KCOU1X = KCOUNT, 1, -1
                  JJ = INT(Y/DS) + 1
                  LOC = (Y-(JJ-1)*DS)/DS
                  UTEMP = LOC*U(KCOUNT,JJ+1) + (1.0-LOC)*U(KCOUNT,JJ)
                  IF (UTEMP .LT. 0.001D0) THEN
                     TTIME(I,J) = 0.0
                     GO TO 4
                  ELSE
                     VTEMP = LOC*V(KCOUNT,JJ+1) + (1.0-LOC)*V(KCOUNT,JJ)
                     IF (VTEMP .LT. 0.0) THEN
                        TTIME(I,J) = 0.0
                        GO TO 4
                     ENDIF
                     Y = Y - VTEMP/UTEMP*DS
                     TTIME(I,J) = TTIME(I,J) + DS/UTEMP
                  ENDIF
                  KCOUNT = KCOUNT - 1
               END DO
            ENDIF
    4       CONTINUE
         END DO
      END DO
      TTIME(1,1) = DS/UO
      DO I = 2, IM
         TTIME(I,1) = TTIME(I-1,1) + DS/U(I,1)
      END DO
      RETURN 
      END
************************************************************************
*       This program solves the equations describing the hemipelagic   *
*       sedmentation under a 2-D plane jet, as modified from           *
*       Syvitski et al. (1988).  Programmed by R. Slingerland, 1991.   *
************************************************************************
************************************************************************
      Subroutine hemi(ttime,im,jm,R)
      Implicit None
      Integer*2 i,j,im,jm,k,ngs,jmd2,LI,LJ
      PARAMETER (LI=20,LJ=20)
      real*8 Co(4),lambda(4),rhosubb(4),ttime(LI,LJ),Ho,con,Ztot,
     #Z(200,200,4),R(LI,LJ),Bo,uo,ds,sflux,xo
      Parameter (ngs=4)
      common/com1/uo,Bo,ds,xo,Ho
      common/com3/Co
      JMD2 = JM/2
      DO I = 1, IM
         DO J = 1, JMD2
            DO K = 1, 4
               Z(I,J,K) = 0.0
            END DO
            R(I,J) = 0.0
         END DO
      END DO
C-----------------------------------------------------------------------
C  Define lambda (1/day), and rhosubb (kg/m**3) for each size fraction
C-----------------------------------------------------------------------
C  Coarse silt
C-----------------------------------------------------------------------
      LAMBDA(1) = 15.0
      RHOSUBB(1) = 1750.0
C-----------------------------------------------------------------------
C  Medium silt
C-----------------------------------------------------------------------
      LAMBDA(2) = 6.0
      RHOSUBB(2) = 1600.0
C-----------------------------------------------------------------------
C  Fine silt
C-----------------------------------------------------------------------
      LAMBDA(3) = 3.0
      RHOSUBB(3) = 1500.0
C-----------------------------------------------------------------------
C  Clay
C-----------------------------------------------------------------------
      LAMBDA(4) = 2.0
      RHOSUBB(4) = 1400.0
      DO I = 1, IM
         DO J = 1, JMD2
            DO K = 1, 4
               CON = LAMBDA(K)/86400.0
C-----------------------------------------------------------------------
C  Calculate the sedimentation rate of the kth size fraction at node i,j
C  in kg/sec/m**2
C-----------------------------------------------------------------------
               IF (TTIME(I,J) .LT. 1.0) THEN
                  Z(I,J,K) = 0.0
               ELSE
                  Z(I,J,K) = CON*HO*CO(K)/1000.0*DEXP((-CON*TTIME(I,J)))
               ENDIF
            END DO
         END DO
      END DO
C-----------------------------------------------------------------------
C  Apportion the sediment flux from the river mouth to each site in
C  proportion to the site's Z value. This insures that mass is
C  conserved, regardless of the grid coarseness.
C  First calculate sflux, half the total sediment flux of each size
C  fraction exiting the river mouth (in kg/sec). Then find Ztot,
C  the total potential sedimentation rate of each size class
C  (in kg/sec-m**2). Next apportion the flux from the river mouth
C  according to a site's proportion of that total.
C-----------------------------------------------------------------------
      DO K = 1, 4
         ZTOT = 0.0
         SFLUX = CO(K)/1000.0*HO*BO/2.0*UO
         DO I = 1, IM
            DO J = 1, JM
               ZTOT = ZTOT + Z(I,J,K)
            END DO
         END DO
         DO I = 1, IM
            DO J = 1, JM
               Z(I,J,K) = SFLUX*Z(I,J,K)/ZTOT
C-----------------------------------------------------------------------
C  Calculate the total thickness of sediment deposited per unit time
C  at i,j
C-----------------------------------------------------------------------
               R(I,J) = R(I,J) + Z(I,J,K)/(RHOSUBB(K)*DS**2)
            END DO
         END DO
      END DO
      RETURN 
      END
************************************************************************
*       This subroutine solves the modified diffusion equation for bed *
*       elevation as a function of time.                               *
*       Programmed by R. Slingerland, 1991.                            *
************************************************************************
************************************************************************
      Subroutine bed(im,jm,t,dt,h,R,Db,ihold)
      Implicit None
      Integer*2   i,j,im,jm,ii,jj,indx(2500),n,np,LI,LJ,ihold
      PARAMETER (LI=20,LJ=20)
      Real*8   A(500,500),b(2500),h(LI,LJ),thetax,thetay,DCx,DCy,D,
     #R(LI,LJ),Db,ds,dt,t,uo,xo,Bo,Ho,clevel
      common/com1/uo,bo,ds,xo,Ho
      common/com2/DCx,DCy,clevel
C------------------------------------------------------------------------
C  Define some constants
C------------------------------------------------------------------------
      N = IM*JM
      NP = 500
C------------------------------------------------------------------------
C  Fill in the coefficient matrix,A
C------------------------------------------------------------------------
      DO II = 1, N
         DO JJ = 1, N
            IF (II .EQ. JJ) THEN
               A(II,JJ) = 1.0
            ELSE
               A(II,JJ) = 0.0
            ENDIF
         END DO
      END DO
C------------------------------------------------------------------------
C  Fill in the right-hand vector, b, and the remaining parts of A
C------------------------------------------------------------------------
      II = 0
      DO J = 1, JM
         DO I = 1, IM
C------------------------------------------------------------------------
C  Turn off diffusion for all nodes landward of shoreline; otherwise
C  set theta
C------------------------------------------------------------------------
            IF (I .LE. IHOLD) THEN
               THETAX = 0.0
               THETAY = 0.0
            ELSE
               THETAX = DCX*DT/(2.0*DS**2)
               THETAY = DCY*DT/(2.0*DS**2)
            ENDIF
            II = II + 1
C------------------------------------------------------------------------
C  The next four equations for b(ii) define the south, west, north, and
C  east boundary conditions, respectively; the fifth equation defines b
C  for all interior nodes; the remainder fill in the coefficient matrix.
C------------------------------------------------------------------------
            IF (J .EQ. 1) THEN
               B(II) = H(I,J+1)
            ELSE IF (I .EQ. 1) THEN
               B(II) = H(I,J) + DT*R(I,J)
            ELSE IF (J .EQ. JM) THEN
               B(II) = H(I,JM-1)
            ELSE IF (I .EQ. IM) THEN
               B(II) = H(IM-1,J)
            ELSE
               B(II) = H(I,J) + THETAX*(H(I+1,J)-2.0*H(I,J)+H(I-1,J)) + 
     #            THETAY*(H(I,J+1)-2.0*H(I,J)+H(I,J-1)) + DT*R(I,J)
               JJ = II
               A(II,JJ) = 1.0 + 2.0*THETAX + 2.0*THETAY
               A(II,JJ-1) = -THETAX
               A(II,JJ+1) = -THETAX
               A(II,JJ-IM) = -THETAY
               A(II,JJ+IM) = -THETAY
            ENDIF
         END DO
      END DO
C------------------------------------------------------------------------
C  Now solve for the new bed elevations by LU decomposition
C------------------------------------------------------------------------
      CALL LUDCMP (A, N, NP, INDX, D)
      CALL LUBKSB (A, N, NP, INDX, B)
      II = 0
      DO J = 1, JM
         DO I = 1, IM
            II = II + 1
            H(I,J) = B(II)
         END DO
      END DO
      RETURN 
      END
************************************************************************
*       Given an NxN matrix A, with physical dimenion NP, this routine *
*       replaces it by the LU decomposition of a rowwise permutation of*
*       itself(Numerical Recipies, 1986; Copyright)                    *
************************************************************************
************************************************************************
      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      Implicit None
      Integer*2 NMAX,NP,N
      Real*8 TINY
      PARAMETER (NMAX=1000,TINY=1.0E-20)
      Real*8 A(NP,NP),VV(NMAX),D,aamax,sum,dum
      Integer*2 indx(n),np,n,j,i,k,imax
      D = 1.
      DO 2 I = 1, N
         AAMAX = 0.
         DO 1 J = 1, N
            AAMAX = AMAX1(ABS(A(I,J)),AAMAX)
    1    CONTINUE
         IF (AAMAX .EQ. 0.) THEN
            PAUSE 'Singular matrix.'
         ENDIF
         VV(I) = 1./AAMAX
    2 CONTINUE
      DO 9 J = 1, N
         IF (J .GT. 1) THEN
            DO 4 I = 1, J - 1
               SUM = A(I,J)
               IF (I .GT. 1) THEN
                  DO 3 K = 1, I - 1
                     SUM = SUM - A(I,K)*A(K,J)
    3             CONTINUE
                  A(I,J) = SUM
               ENDIF
    4       CONTINUE
         ENDIF
         AAMAX = 0.
         DO 6 I = J, N
            SUM = A(I,J)
            IF (J .GT. 1) THEN
               DO 5 K = 1, J - 1
                  SUM = SUM - A(I,K)*A(K,J)
    5          CONTINUE
               A(I,J) = SUM
            ENDIF
            DUM = VV(I)*ABS(SUM)
            IF (DUM .GE. AAMAX) THEN
               IMAX = I
               AAMAX = DUM
            ENDIF
    6    CONTINUE
         IF (J .NE. IMAX) THEN
            DO 7 K = 1, N
               DUM = A(IMAX,K)
               A(IMAX,K) = A(J,K)
               A(J,K) = DUM
    7       CONTINUE
            D = -D
            VV(IMAX) = VV(J)
         ENDIF
         INDX(J) = IMAX
         IF (J .NE. N) THEN
            IF (A(J,J) .EQ. 0.) A(J,J) = 1.0E-20
            DUM = 1./A(J,J)
            DO 8 I = J + 1, N
               A(I,J) = A(I,J)*DUM
    8       CONTINUE
         ENDIF
    9 CONTINUE
      IF (A(N,N) .EQ. 0.) A(N,N) = 1.0E-20
      RETURN 
      END
************************************************************************
*       Solves the set of n linear equations AX=B where A is from      *
*       Subroutine LUDCMP                                              *
*       (Numerical Recipies, 1986; Copyright)                          *
************************************************************************
************************************************************************
      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      Implicit None
      Integer*2 N,NP
      real*8 A(NP,NP),B(N),SUM
      Integer*2 INDX(N),II,I,N,LL,J,NP
      II = 0
      DO 2 I = 1, N
         LL = INDX(I)
         SUM = B(LL)
         B(LL) = B(I)
         IF (II .NE. 0) THEN
            DO 1 J = II, I - 1
               SUM = SUM - A(I,J)*B(J)
    1       CONTINUE
         ELSE IF (SUM .NE. 0.) THEN
            II = I
         ENDIF
         B(I) = SUM
    2 CONTINUE
      DO 4 I = N, 1, -1
         SUM = B(I)
         IF (I .LT. N) THEN
            DO 3 J = I + 1, N
               SUM = SUM - A(I,J)*B(J)
    3       CONTINUE
         ENDIF
         B(I) = SUM/A(I,I)
    4 CONTINUE
      RETURN 
      END
