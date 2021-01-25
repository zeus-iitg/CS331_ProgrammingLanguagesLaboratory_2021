/* 

Instructions for Running the Code:

For Linux:
1. Make sure Java is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'javac partA.java' and press enter. 
4. To run the file, type 'java partA' and press enter.
5. You will be prompted to enter the number of threads.
6. Enter a number between 4 and 16, both inclusive and press enter.
7. If a number out of this range is entered, an error message will be printed and the program will exit.

For Windows:
1. Make sure Java is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'javac partA.java' and press enter. 
4. To run the file, type 'java partA' and press enter.
5. You will be prompted to enter the number of threads.
6. Enter a number between 4 and 16, both inclusive and press enter.
7. If a number out of this range is entered, an error message will be printed and the program will exit.

*/

//importing the necessary packages required
//for using Scanner and Atomic Integer
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicInteger;

//declaring a class named Counter having
//an atomic integer as a global variable
class Counter {
    //This will store the number of points inside the circle
    //Every thread, after it completes its execution, will
    //add its local count to this variable
    public static AtomicInteger numInsideCircleAtomicInteger = new AtomicInteger(0);
}

class Multithreading implements Runnable {
    int remainder; //this thread will compute the points whose index % numThreads = remainder
    int numThreads; //stores the value of total number of threads
    int inside; //local variable storing the number of points inside the circle for this thread

    //constructor
    public Multithreading(int n, int rem) {
        numThreads = n;
        remainder = rem;
        inside = 0;
    }

    public void run() {
        for (int i = remainder; i < 1000000; i += numThreads) {
            //generate random x-coordinate value between 0 and 1 
            double x = Math.random();
            //generate random y-coordinate value between 0 and 1
            double y = Math.random();
            //compute euclidean distance
            double dist = x * x + y * y;
            //if distance from origin <= 1, point lies inside circle
            if (Double.compare(dist, (double)1) <= 0)
                inside++;
        }
        //add local value to the global variable
        //using Atomic Integer function calls
        Counter.numInsideCircleAtomicInteger.updateAndGet(n -> n + inside);
    }
}

public class partA {
    public static void main(String[] args) {
        //Taking number of threads as input
        System.out.println("Input number of threads");

        //declaring a new scanner object
        Scanner sc = new Scanner(System.in);
        //saving number of threads in a variable named numThreads
        int numThreads = sc.nextInt();
        //closing the scanner object
        sc.close();

        //checking if number of threads is in the specified range
        if(numThreads < 4 || numThreads > 16){
            System.out.println("Number of threads should be between 4 and 16, both inclusive");
            System.out.println("Exiting program");
            System.exit(0);
        }

        //storing the start time of execution of program
        long startTime = System.nanoTime();

        //declaring an array of threads
        Thread thArray[] = new Thread[numThreads];

        for (int i = 0; i < numThreads; i++) {
            //declaring a new object of multithreading class
            //with parameters numThreads and i
            Multithreading m = new Multithreading(numThreads, i);
            //making a new thread with this object and
            //storing it in the thread array
            thArray[i] = new Thread(m);
        }

        //starting all threads
        for (int i = 0; i < numThreads; i++) {
            thArray[i].start();
        }

        //waiting till all threads have finished execution
        for (int i = 0; i < numThreads; i++) {
            try {
                thArray[i].join();
            } catch (InterruptedException e) {
                System.out.println (e);
            }
        }

        System.out.println("Number of points inside circle : " + Counter.numInsideCircleAtomicInteger.get());
        System.out.println("Total number of points : " + 1000000);

        //Using the formula
        //numInsideCircle/numTotal = PI/4
        //to calculate the approximate value of PI
        double numerator = (double) (4 * Counter.numInsideCircleAtomicInteger.get());
        double denominator = (double) 1000000;

        double PI = numerator / denominator;

        //printing the computed value of PI
        System.out.println("Approximate value of PI : " + PI);

        //storing the end time of execution of program
        long endTime = System.nanoTime();

        //calculating the running time of program
        long timeElapsed = endTime - startTime;
        //printing the running time in milliseconds
        System.out.println("Execution time in milliseconds : " + timeElapsed / 1000000);
    }
}