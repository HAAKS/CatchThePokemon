location(2,0,1,T,s0):- T=<0.
north(1,0).
north(1,1).
north(1,2).
north(2,0).
north(2,1).
north(2,2).
south(0,0).
south(0,1).
south(0,2).
south(1,0).
south(1,1).
south(1,2).
east(0,0).
east(2,1).

west(0,1).
hasPokemon(2,1).



location(X,Y,P,T,result(A,S)):-  
        
        (T1 is T-1 ,P1 is P-1, X1 is X+1, A=up ,  (north(X1,Y)) ,hasPokemon(X,Y,S),location(X1,Y,P1,T1,S)); 
        (T1 is T-1 ,X1 is X+1, A=up ,(north(X1,Y)),\+hasPokemon(X,Y,S) ,  location(X1,Y,P,T1,S));

        
        (T1 is T-1 , P1 is P-1, X2 is X-1, A=down , (south(X2,Y)), hasPokemon(X,Y,S) ,location(X2,Y,P1,T1,S)); 
        (T1 is T-1 , X2 is X-1, A=down ,(south(X2,Y)), \+hasPokemon(X,Y,S) , location(X2,Y,P,T1,S)) ;

        
        (T1 is T-1 , P1 is P-1, Y1 is Y-1,A=right ,(east(X,Y1)),hasPokemon(X,Y,S) ,  location(X,Y1,P1,T1,S)) ;
        (T1 is T-1 , Y1 is Y-1,A=right , (east(X,Y1)), \+hasPokemon(X,Y,S) ,location(X,Y1,P,T1,S)) ;

    
        (T1 is T-1 , P1 is P-1, Y2 is Y+1, A=left , (west(X,Y2)),hasPokemon(X,Y,S) , location(X,Y2,P1,T1,S));
        (T1 is T-1 , Y2 is Y+1, A=left ,(west(X,Y2)),\+hasPokemon(X,Y,S),  location(X,Y2,P,T1,S) );
        
        (A=up ,X1 is X+1,\+north(X1,Y),location(X,Y,P,T,S)) ;
        (A=down ,X2 is X-1,\+south(X2,Y),location(X,Y,P,T,S) );
        (A=left ,Y1 is Y+1,\+west(X,Y1),location(X,Y,P,T,S) );
        (A=right ,Y2 is Y-1,\+east(X,Y2),location(X,Y,P,T,S) ).

hasPokemon(X,Y,result(A,S)) :- 
            hasPokemon(X,Y,S) ,
            (
           (X1 is X+1, A=up ,north(X1,Y)) ;
            (X2 is X-1, A=down ,(south(X2,Y))) ;
            (Y1 is Y-1,A=right ,(east(X,Y1))) ;
             (Y2 is Y+1, A=left ,(west(X,Y2)))).

checkTheDepth(L,X,R) :-
call_with_depth_limit(L,X,R),
R \= depth_limit_exceeded;
Y is X+1,
checkTheDepth(L,Y,R).

