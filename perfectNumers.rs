use std::io;

pub fn perfect(max: u32) {
    // Comprobamos que el numero introducido no sea 0
    if max == 0 { println!("Tu eres tonto o naciste haciendo puenting? '-'"); }

    // El numero es positivo, luego podemos continuar
    else{
        let mut perfectos = Vec::<u32>::new(); // Vector de numeros perfectos
        let mut sumador: u32;

        for i in 1..=max {
            let mut divisores = Vec::new(); // Vector de los divisores de cada numero (i)
            if i == 1 || i == 2 || (i%10 != 6 && i%10 != 8) { continue; } // Los numeros perfectos siempre terminan en 6 u 8

            else {
                for j in 1..=i/2 { // Probamos solo hasta la mitad del numero elegido
                    if i % j == 0 { divisores.push(j); }
                }
                sumador = divisores.iter().sum();
                if sumador == i { perfectos.push(i); }
            }
        }
        if perfectos.is_empty() { println!("Pues no habia ninguno perfecto en ese rango jaja nub"); }
        else { println!("Los numeros perfectos son: {:?}", perfectos); }
    }
}

fn main(){
    println!("Hola");
    println!("Vamos a ver si esto tira, mete un numero: ");

    // Sirve para leer un numero por pantalla. Al parsear con u32 evitamos los numeros negativos.
    let mut input_text = String::new();
    io::stdin()
        .read_line(&mut input_text)
        .expect("failed to read from stdin");

    let trimmed = input_text.trim();
    match trimmed.parse::<u32>() {
        Ok(i) => perfect(i),
        Err(..) => println!("this wasn't a not negative integer: {}", trimmed),
    };
}