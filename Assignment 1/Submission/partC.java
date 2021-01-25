/* 

Instructions for Running the Code:

For Linux:
1. Make sure Java is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'javac partC.java' and press enter. 
4. To run the file, type 'java partC' and press enter.
5. You will be prompted to enter the number of threads.
6. Enter a number between 4 and 16, both inclusive and press enter.
7. If a number out of this range is entered, an error message will be printed and the program will exit.
8. There is an option available for printing the matrices in files. For Linux, refer to line number 245.

For Windows:
1. Make sure Java is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'javac partC.java' and press enter. 
4. To run the file, type 'java partC' and press enter.
5. You will be prompted to enter the number of threads.
6. Enter a number between 4 and 16, both inclusive and press enter.
7. If a number out of this range is entered, an error message will be printed and the program will exit.
8. There is an option available for printing the matrices in files. For Windows, refer to line number 227.

*/

//importing the necessary packages required for
//using Scanner, File, FileWriter and IOException
import java.util.Scanner;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

//declaring a class named Counter having 3
//1000 x 1000 matrices named A, B and C of type double
//Matrix C is the matrix multiplication of A and B
//C = A x B
class Counter {
    public static double A[][] = new double[1000][1000];
    public static double B[][] = new double[1000][1000];
    public static double C[][] = new double[1000][1000];
}

class Multithreading implements Runnable {
    int start; //stores the starting index of cells to be calculated by this thread
    int end; //stores the ending index of cells to be calculated by this thread
    //type denotes the type of operation to be performed
    //1 denotes initialize matrix A
    //2 denotes initialize matrix B
    //3 denotes compute matrix C = A x B
    int type;
    int size; //size denotes the dimension of square matrix
    //upper_limit denotes the upper limit for values in the matrix
    //lower limit is taken as 0 by default
    int upper_limit;

    //constructor
    public Multithreading(int st, int en, int t, int sz, int lim) {
        start = st;
        end = en;
        type = t;
        size = sz;
        upper_limit = lim;
    }

    public void run() {
        //initialize matrix A
        if(type == 1){
            for (int i = start; i <= end; i++) {
                //extract row and column from cell number
                int row = i / size;
                int col = i % size;
                //set value of A[row][col] to a random value
                Counter.A[row][col] = upper_limit * Math.random();
            }
        }
        //initialize matrix B
        else if(type==2){
            for (int i = start; i <= end; i++) {
                //extract row and column from cell number
                int row = i / size;
                int col = i % size;
                //set value of B[row][col] to a random value
                Counter.B[row][col] = upper_limit * Math.random();
            }
        }
        //compute matrix C
        else{
            for (int i = start; i <= end; i++) {
                //extract row and column from cell number
                int row = i / size;
                int col = i % size;
                //calculate value of C[row][col]
                double sum = 0;
                for(int j = 0; j < size; j++)
                    sum += Counter.A[row][j] * Counter.B[j][col];
                //set value of C[row][col]
                Counter.C[row][col]  = sum;
            }
        }
    }
}

public class partC {
    //for printing matrices to files
    //type denotes the type of operation to be performed
    //1 denotes print matrix A
    //2 denotes print matrix B
    //3 denotes print matrix C
    //filename denoted name of the output file
    //sz denotes the dimension of square matrix
    static void print(int type, String filename, int sz) throws IOException{
        //creating a new File object named output
        //which opens the file whose name is
        //stored inside the String variable filename
        File output = new File(filename);

        //creating a new FileWriter object and
        //linking it to newly created File object
        FileWriter writer = new FileWriter(output);
        //Strings denoting whitespace and newline characters
        String whitespace = " ";
        String newline = "\n";
        for(int i = 0; i < sz; i++){
            for(int j = 0; j < sz; j++){
                double value;
                if(type == 1)
                    value = Counter.A[i][j];
                else if(type == 2)
                    value = Counter.B[i][j];
                else
                    value = Counter.C[i][j];
                //writing the output based on operation type
                String out = Double.toString(value);
                writer.write(out);
                writer.write(whitespace);
            }
            writer.write(newline);
        }
        //flushing the writer object to ensure that
        //every character is properly written to file
        writer.flush();
        //closing the writer object
        writer.close();

    }

    public static void main(String[] args) throws IOException{
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

        System.out.println("Matrix Multiplication begins");

        //storing the start time of execution of program
        long startTime = System.nanoTime();

        //variable storing dimension of square matrix
        int sz = 1000;
        //variable storing upper limit of values inside matrix
        //lower limit is taken to be 0 by default
        int lim = 10;
        //variable storing total number of cells
        int numValues = sz * sz;
        //variable storing number of cells assigned per thread
        int valuesPerThread = (numValues + numThreads - 1) / numThreads;

        //declaring an array of threads
        Thread thArray[] = new Thread[numThreads];
        
        for(int type = 1; type <= 3; type++){
            //variable storing index of last cell assigned to a thread
            int last = -1;
            for (int i = 0; i < numThreads; i++) {
                //variable storing starting index of cells assigned to this thread
                int start = last + 1;
                //variable storing ending index of cells assigned to this thread
                int end = last + valuesPerThread;
                //making sure ending index doesnt exceed total number of cells
                end = Math.min(end, numValues - 1);
                //declaring a new object of multithreading class
                //with parameters start, end, type, sz and lim
                Multithreading m = new Multithreading(start, end, type, sz, lim);
                //making a new thread with this object and
                //storing it in the thread array
                thArray[i] = new Thread(m);
                //updating the last cell assigned to any thread as end
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
        }
        //storing the end time of execution of program
        long endTime = System.nanoTime();

        //calculating the running time of program
        long timeElapsed = endTime - startTime;
        System.out.println("Matrix Multiplication ends");
        System.out.println("Execution time in milliseconds : " + timeElapsed / 1000000);
        
        //For printing output files
        //For Windows, uncomment this
        /*
        System.out.println("Writing output to files begins");
        long startTime_out_Win = System.nanoTime();
        
        print(1, ".\\A.txt", sz);
        print(2, ".\\B.txt", sz);
        print(3, ".\\C.txt", sz);

        long endTime_out_Win = System.nanoTime();

        long timeElapsed_out_Win = endTime_out_Win - startTime_out_Win;
        System.out.println("Writing output to files ends");
        System.out.println("Execution time in milliseconds : " + timeElapsed_out_Win / 1000000);
        long totalTimeElapsed_Win = endTime_out_Win - startTime;
        System.out.println("Execution time in milliseconds : " + totalTimeElapsed_Win / 1000000);
        */

        //For Linux, uncomment this
        /*
        System.out.println("Writing output to files begins");
        long startTime_out_Lin = System.nanoTime();
        
        print(1, "./A.txt", sz);
        print(2, "./B.txt", sz);
        print(3, "./C.txt", sz);

        long endTime_out_Lin = System.nanoTime();

        long timeElapsed_out_Lin = endTime_out_Lin - startTime_out_Lin;
        System.out.println("Writing output to files ends");
        System.out.println("Execution time in milliseconds : " + timeElapsed_out_Lin / 1000000);
        long totalTimeElapsed_Lin = endTime_out_Lin - startTime;
        System.out.println("Execution time in milliseconds : " + totalTimeElapsed_Lin / 1000000);
        */

    }
}