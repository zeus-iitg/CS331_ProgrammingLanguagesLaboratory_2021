/* 

Instructions for Running the Code:

For Linux:
1. Make sure Java is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'javac partB.java' and press enter. 
4. To run the file, type 'java partB' and press enter.
5. You will be prompted to enter the number of threads.
6. Enter a number between 4 and 16, both inclusive and press enter.
7. If a number out of this range is entered, an error message will be printed and the program will exit.

For Windows:
1. Make sure Java is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'javac partB.java' and press enter. 
4. To run the file, type 'java partB' and press enter.
5. You will be prompted to enter the number of threads.
6. Enter a number between 4 and 16, both inclusive and press enter.
7. If a number out of this range is entered, an error message will be printed and the program will exit.

*/

//importing the necessary packages
//required for using Scanner
import java.util.Scanner;

//declaring a class named Counter having
//a double named sum as a global variable
class Counter {
    public static double sum = 0;
    //declaring a synchronized method inside Counter class
    //which will add the value given in argument to sum
    synchronized public static void add(double value) {
        sum += value;
    }
}

class Multithreading implements Runnable {
    int start; //stores the starting index (1-indexed) of points to be calculated by this thread
    int end; //stores the ending index (1-indexed) of points to be calculated by this thread
    double intervalLength; //stores length of interval
    int numPoints; //stores the total number of points
    double sum; //local variable storing sum of values computed by this thread

    //constructor
    public Multithreading(int st, int en, double len, int num) {
        start = st;
        end = en;
        intervalLength = len;
        numPoints = num;
        sum = 0;
    }

    public void run() {
        //iterate over all points with index between start and end, both inclusive
        for (int i = start; i <= end; i++) {
            //computing x-coordinate of point corresponding to this index
            double x = -1 + intervalLength * (i - 1);
            //calculating exponent corresponding to this value of x
            double exponent = -1 * ((x * x) / 2);
            //computing value
            double value = Math.exp(exponent);
            //multiplying this value with simpson's coefficient
            if(i != 1 && i != numPoints){
                if(i % 2 == 1) {
                    value = value * 2;
                }
                else {
                    value = value *  4;
                }
            }
            //adding this to the local variable
            sum += value;
        }
        //adding the local value to the global variable
        Counter.add(sum);
    }
}

public class partB {
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

        //variable to store number of partitions
        int numPartitions = 2000000;
        //variable to store length of interval
        double intervalLength = (double) 2 / numPartitions;

        //variable storing total number of points
        int numPoints = numPartitions + 1;

        //declaring an array of threads
        Thread thArray[] = new Thread[numThreads];
        
        //variable storing points to compute per thread
        int pointsPerThread = (numPoints + numThreads - 1) / numThreads;
        //variable storing last point assigned to a thread
        int last = 0;
        for (int i = 0; i < numThreads; i++) {
            //variable storing starting index of points to be computed by this thread
            int start = last + 1;
            //variable storing ending index of points to be computed by this thread
            int end = last + pointsPerThread;
            //making sure ending index doesnt exceed number of points
            end = Math.min(end, numPoints);
            //declaring a new object of multithreading class
            //with parameters start, end, intervalLength and numPoints
            Multithreading m = new Multithreading(start, end, intervalLength, numPoints);
            //making a new thread with this object and
            //storing it in the thread array
            thArray[i] = new Thread(m);
            //updating the last point assigned to any thread as end
            last = end;
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

        //printing the number of partitions
        System.out.println("Number of partitions : " + numPartitions);

        //dividing the current result by sqrt(2*PI)
        double numerator = Counter.sum;
        double denominator = Math.sqrt(2 * Math.PI);
        double integral = numerator / denominator;
        //multiplying the current result by intervalLength/3
        integral = (integral * intervalLength) / 3;
        System.out.println("Computed value of integral : " + integral);

        //storing the end time of execution of program
        long endTime = System.nanoTime();

        //calculating the running time of program
        long timeElapsed = endTime - startTime;
        //printing the running time in milliseconds
        System.out.println("Execution time in milliseconds : " + timeElapsed / 1000000);
    }
}