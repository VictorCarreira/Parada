PROGRAM MQNL

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
    !Programa do teste do critério de parada da RNA                              !
    !Orientador: Cosme Ferreira da Ponte Neto                                    !
    !Aluno: Victor Ribeiro Carreira                                              !
    !Categoria: RNA                                                              !
    !Objetivo:                                                                   !
    !Calcular A, B e C através do método dos mínimos quadrados não lineares, de  !
    !            modo a ajustar os dados abaixo a uma função exponencial do tipo:!
    !            y=A.{e}^{−Bx}+C.                                                !
    !            Fazer uma rotina em FORTRAN para ser futuramente aproveitada no !
    !            critério de parada de uma rede                                  !
    !Para usar compilação com flags utilize:                                     !
    !gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic                 !
    !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                     !
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

                          !***********TABELA DE VARIÁVEIS***********!
                          !Conv: matriz de dados de convergência da !
                          !       rede                              !      
                          !-----------------------------------------!

  IMPLICIT NONE

  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: ie,jj,i,j,k
  INTEGER(KIND=SP):: neq, npar,nit

  REAL(KIND=DP):: inicio,final, A0, B0, C0
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: f, p, f1, p0, x
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, AT, ATA, C
  REAL(KIND=DP), PARAMETER:: lambda=0.9d0
  
  CALL CPU_TIME(inicio)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!LENDO OS ARQUIVOS DE ENTRADA!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
   OPEN(1,FILE='dado.txt')
   OPEN(2,FILE='saida.txt')

   
  
   ie=1  !Contador de linhas do dado de entrada
   DO WHILE (.TRUE.)
     READ(1,*,END=8) ie
     ie=ie+1
   END DO
 8   CONTINUE
   CLOSE(1)

   nit=10000 ! número de iterações
   npar=3 ! A, B e C
   neq= ie-1 ! Número de eq. do sistema não-lienar
  
   ALLOCATE(A(neq,npar),AT(npar,neq),ATA(npar,npar),C(npar,neq))
   ALLOCATE(x(neq),f(neq),p(npar),f1(neq),p0(npar))

    WRITE(6,*)"A=", A
    !PRINT 21, x
    !PRINT 20, ie

    A0=55d0 ! valores iniciais do parâmetro A
    B0=0.110d0 ! valores iniciais do parâmetro B
    C0=1.1d0 ! valores iniciais do parâmetro C

    DO jj=1,nit !laço das iterações. Corresponde a cada ponto, em linha, do dado. 
      DO i=1,neq ! Aqui é onde é montada a matriz Jacobiana (derivada parcial em relação aos parâmetros). 
       A(i,1)=EXP(-B0*x(i)) !Derivada parcial da função em relação ao parâmetro A
       A(i,2)=A0*EXP(-B0*x(i)*(-x(i))) !Derivada parcial da função em relação ao parâmetro B
       A(i,3)=1d0 !Derivada parcial da função em relação ao parâmetro C     
      END DO
    
      DO i=1,neq 
       DO j=1,npar
        AT(j,i)=A(i,j) !Transpondo a matriz Jacobiana e armazenando em AT
       END DO
      END DO

      DO k=1,npar
       DO j=1,npar
        ATA(j,k)=0.0d0
         DO i=1,neq
          ATA(j,k)=ATA(j,k)+AT(j,i)*A(i,k) ! Faz a produto matricial AT.A
         END DO 
       END DO
      END DO     
     
      DO i=1,3
       ATA(i,i)=ATA(i,i)+lambda !Adiciona o multiplicador de lagrange ou suavidade. 
      END DO 

      CALL INVERT(ATA,npar) ! Subrotina que inverte ATA

      DO k=1,neq
       DO j=1,npar
        C(j,k)=0.0d0
         DO i=1,npar
          C(j,k)=C(j,k)+ATA(j,i)*AT(i,k) !Multiplica a inversa de ATA por AT e iguala a C
         END DO
       END DO
      END DO  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!FORMATO DOS ARQUIVOS DE SAÍDA!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   20 FORMAT(I3)
   21 FORMAT(ES12.2)
   22 FORMAT(ES12.2,1x,ES12.2)

!------------------------------------------------------------------------------!

  CALL CPU_TIME(final)
  PRINT*,'tempo de máquina=',final-inicio
  !*******************************************************************************************!
  CONTAINS

  

     SUBROUTINE INVERT(A,i)
        integer i,im,j,k,l
        real*8 A(i,i),B(i)

         IM=I-1

         DO 5 K=1,I
           DO 2 J=1,IM
             2 B(J)=A(1,J+1)/A(1,1)
             B(I)=1.d0/A(1,1)
             DO 4 L=1,IM
               DO 3 J=1,IM
                 3 A(L,J)=A(L+1,J+1)-A(L+1,1)*B(J)
                 4 A(L,I)=-A(L+1,1)*B(I)
                 DO 5 J=1,I
                   5 A(I,J)=B(J)

     END SUBROUTINE INVERT




END PROGRAM MQNL