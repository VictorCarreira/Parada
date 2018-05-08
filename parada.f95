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
  INTEGER(KIND=SP):: ie
  INTEGER(KIND=SP):: neq, npar

  REAL(KIND=DP):: inicio,final
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: f, p, f1, p0, x
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, AT, ATA, C
  !REAL(KIND=DP), PARAMETER::
  
  CALL CPU_TIME(inicio)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!LENDO OS ARQUIVOS DE ENTRADA!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
   OPEN(1,FILE='dado.txt')
   OPEN(2,FILE='saida.txt')

   
  
   ie=1  !Contador do dado de entrada
   DO WHILE (.TRUE.)
     READ(1,*,END=8) x(ie), f(ie) 
     ie=ie+1
   END DO
 8   CONTINUE
   CLOSE(1)


   npar=3 ! A, B e C
   neq= ie-1 ! Número de eq. do sistema não-lienar
  
   ALLOCATE(A(neq,npar),AT(npar,neq),ATA(npar,npar),C(npar,neq))
   ALLOCATE(x(neq),f(neq),p(npar),f1(neq),p0(npar))




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!FORMATO DOS ARQUIVOS DE SAÍDA!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  15 FORMAT(A9,5x,A6,6x,A4,2x,A4,7x,A4,7x,A3,8x,A3)
!  21 FORMAT(A9,2x,E12.2)
!  22 FORMAT(A1,1x,A1)

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