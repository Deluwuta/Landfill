public class hamilton {
    int grafo[][] = {{0, 1, 0, 0, 1},
                     {1, 0, 1, 1, 1},
                     {0, 1, 0, 1, 1},
                     {0, 1, 1, 0, 1},
                     {1, 1, 1, 1, 0}};

    int grafo2[][] ={{0, 1, 0, 0, 0, 1},
                     {1, 0, 1, 0, 0, 0},
                     {0, 1, 0, 1, 1, 1},
                     {0, 0, 1, 0, 1, 0},
                     {0, 0, 1, 1, 0, 0},
                     {1, 0, 1, 0, 0, 0}};

    int vectorSol[] = {0, -1, -1, -1, -1, -1}; // Almacenamos el ciclo, debe empezar y acabar en el mismo vértice
    int N = 5;

    public hamilton(){

    }

    public void mostrarSol(){
        for(int i = 0; i < vectorSol.length; i++)
            System.out.print(vectorSol[i] + " ");
        System.out.println();
    }

    boolean validarSol(){ // Mirar aristas
        int i = 0;
        int j = 1;
        if(vectorSol[0] == vectorSol[N]){
            while(j <= N){
                if(grafo[vectorSol[i]][vectorSol[j]] == 0) return false;
                i++;
                j++;
            }
        }
        else return false;
        return true;
    }

    boolean aceptable(int etapa, int num){
        if(etapa == N && num == vectorSol[0]) return true;

        if(grafo[etapa-1][num] == 1){
            for(int i = 0; i < etapa; i++){
                if(vectorSol[i] == num) return false;
            }
        }
        else return false;

        return true;
    }

    public boolean backtrack(int etapa) {
        if (etapa == N + 1)
            if (validarSol())
                return true;
            else
                return false;
        else {
            for (int i = 0; i < N; i++)
                if (aceptable(etapa, i)) {
                    vectorSol[etapa] = i;
                    if (backtrack(etapa + 1))
                        return true;
                    else {
                        vectorSol[etapa] = -1;
                    }
                }
            return false;
        }
    }

    public static void main(String[] args) {
        hamilton h = new hamilton();
        boolean kek = false;
        kek = h.backtrack(1);
        if(kek) h.mostrarSol();
        else System.out.println("Perra mala");
    }
}
