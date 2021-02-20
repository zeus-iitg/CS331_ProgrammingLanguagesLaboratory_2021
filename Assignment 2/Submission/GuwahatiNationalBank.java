/* 

Instructions for Running the Code
---------------------------------

For Linux:
1. Make sure Java is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'javac GuwahatiNationalBank.java' and press enter. 
4. Make sure 'testcase.txt' is present in the current directory.
5. To run the file, type 'java GuwahatiNationalBank' and press enter.
6. There is an option for printing bank account details of all branches on line 707.

For Windows:
1. Make sure Java is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'javac GuwahatiNationalBank.java' and press enter. 
4. Make sure 'testcase.txt' is present in the current directory.
5. To run the file, type 'java GuwahatiNationalBank' and press enter.
6. There is an option for printing bank account details of all branches on line 707.

*/

//importing the necessary packages
//required to run the program
import java.util.*;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

//declaring a class named GuwahatiNationalBank
//which will contain all the necessary resources
//required to run the simulation program
public class GuwahatiNationalBank {

	//an atomic integer named counter which
	//stores the count of finished tasks
	static AtomicInteger counter;

    //linked list containing accounts of various branches of GNB
    @SuppressWarnings("unchecked") static LinkedList<Account> branchAccounts[] = new LinkedList[10];

    //branch updaters of various branches of GNB
    static branchUpdater br_updater[] = new branchUpdater[10];

    //array storing number of opened accounts in every branch
    //contains count of accounts which were later closed too
    static int num_accounts_in_branch[] = new int[10];

    //a static block which initializes all the static
    //data members of this class 
    static{
        for(int i = 0; i < 10; i++){
            br_updater[i] = new branchUpdater(10);
            branchAccounts[i] = new LinkedList<Account>();
            num_accounts_in_branch[i] = 0;
        }
        counter = new AtomicInteger(0);
    }
    
    //a thread safe method which increments the value of "num_accounts_in_branch"
    //for the branch whose index is "br_num" and returns the previous value
    synchronized public static int getAndIncrementNumAccounts(int br_num){
        int temp = num_accounts_in_branch[br_num];
        num_accounts_in_branch[br_num]++;
        return temp;
    }

    //a class which stores all the necessary details required in a request
    public static class Request {
    	//stores the request id
        int request_number;
        //stores the source account number
        String account_number;
        //stores the destination account number
        String dest_account_number;
        //stores the request type
        int type;
        //stores the transaction amount
        double transaction_amount;
    	
    	//constructor
        public Request(int req_no, String acc_num, String dest_acc_num, int t, double trans_amt) {
            request_number = req_no;
            account_number = acc_num;
            dest_account_number = dest_acc_num;
            type = t;
            transaction_amount = trans_amt;
        }
    }

    //function for printing the account details of all the accounts present
    //in a branch wise manner
    public static void printAccountDetails(){
    	for(int i = 0; i < 10; i++){
    		System.out.println("Branch " + i);
    		for(int j = 0; j < branchAccounts[i].size(); j++){
    			String acc_num = branchAccounts[i].get(j).get_account_number();
    			double balance = branchAccounts[i].get(j).get_balance();
    			System.out.println(acc_num + " " + balance);
    		}
    	}
    }
    
    //a class which implements the Runnable interface
    //which will be used by our threads
    static class Multithreading implements Runnable {
    	//stores the request details that this thread is currently serving
        Request req;
    	
    	//constructor
        public Multithreading(Request r) {
            req = r;
        }
    	
    	//the function that will be called when the thread starts execution
        public void run() {
        	//if it is deposit or withdraw request
        	//traverse over the accounts of source branch
        	//and change the account details on encountering the source account
            if(req.type == 1 || req.type == 2){
                String acc_number = req.account_number;
                int branch_number = Character.getNumericValue(acc_number.charAt(0));
                for(int i = 0; i < branchAccounts[branch_number].size(); i++){
                	if(branchAccounts[branch_number].get(i).get_account_number().equals(acc_number)){
                    	branchAccounts[branch_number].get(i).modify(req);
                        break;
                    }
                }
                //release lock associated with this account number
                br_updater[branch_number].releaseLock(acc_number);
                //increment the value of finished tasks
                counter.getAndIncrement();
            }
            //if it is transfer money request
            //we fulfill this request by breaking it into two requests
            //1. withdraw from source account
            //2. deposit to destination account
            //the result of the first request (SUCCESS / FAILURE) is required in order for
            //the second request to be executed
            if(req.type == 3){
            	//if it is the first request
            	//traverse over the accounts of source branch
        		//and try to withdraw amount on encountering the source account
        		//notify the second request about the success or failure of
        		//the first request
            	if(!req.dest_account_number.equals("-1")){
	                String acc_number = req.account_number;
	                int branch_number = Character.getNumericValue(acc_number.charAt(0));
	                for(int i = 0; i < branchAccounts[branch_number].size(); i++){
	                    if(branchAccounts[branch_number].get(i).get_account_number().equals(acc_number)){
	                        int ret = branchAccounts[branch_number].get(i).modify(req);
	                        String dest_acc_number = req.dest_account_number;
	                        int dest_branch_number = Character.getNumericValue(dest_acc_number.charAt(0));
	                        if(ret == 1)
	                        	br_updater[dest_branch_number].notifyTransactionOutcome(req.request_number, true);
	                        else
	                        	br_updater[dest_branch_number].notifyTransactionOutcome(req.request_number, false);

	                        break;
	                    }
	                }
	                //release lock associated with this account number
	                br_updater[branch_number].releaseLock(acc_number);
	            }
	            //if it is the second request
	            //change its type to deposit request
	            //traverse over the accounts of destination branch
        		//and change the account details on encountering the destination account
	            else{
	            	req.type = 1;
	            	String acc_number = req.account_number;
	                int branch_number = Character.getNumericValue(acc_number.charAt(0));
	                for(int i = 0; i < branchAccounts[branch_number].size(); i++){
	                    if(branchAccounts[branch_number].get(i).get_account_number().equals(acc_number)){
	                        branchAccounts[branch_number].get(i).modify(req);
	                        break;
	                    }
	                }
	                //release lock associated with this account number
	                br_updater[branch_number].releaseLock(acc_number);
	                //increment the value of finished tasks
	                counter.getAndIncrement();
	            }
            }
            //if it is add account request
            //create a valid account having account number 1 more than
            //the last account created in this branch
            //and with the specified initial amount as balance
            //add this account to the linked list of specified branch number
            if(req.type == 4){
                int branch_number = Character.getNumericValue(req.account_number.charAt(0));
                int temp = getAndIncrementNumAccounts(branch_number) + 1;
                String acc_num = String.valueOf(temp);
                while(acc_num.length() < 9){
                    acc_num = "0" + acc_num;
                }
                acc_num = String.valueOf(branch_number) + acc_num;
                branchAccounts[branch_number].add(new Account(acc_num, req.transaction_amount));
                //release lock associated with this branch
                br_updater[branch_number].releaseBranchLock();
                //increment the value of finished tasks
                counter.getAndIncrement();
            }
            //if it is delete account request
            //traverse over the accounts of source branch
            //and remove the account from linked list on encountering it
            if(req.type == 5){
                String acc_number = String.valueOf(req.account_number);
                int branch_number = Character.getNumericValue(acc_number.charAt(0));
                for(int i = 0; i < branchAccounts[branch_number].size(); i++){
                    if(branchAccounts[branch_number].get(i).get_account_number().equals(acc_number)){
                        branchAccounts[branch_number].remove(i);
                        break;
                    }
                }
                //release lock associated with this branch
                br_updater[branch_number].releaseBranchLock();
                //increment the value of finished tasks
                counter.getAndIncrement();
            }
            //if it is transfer account request
            //we fulfill this request by breaking it into two requests
            //1. delete source account
            //2. add an account in destination branch with same balance
            //the balance of the source account needs to be communicated
            //to the second request in order to start its execution
            if(req.type == 6){
            	//if it is the first request
            	//traverse over the accounts of source branch
           		//and remove the account from linked list on encountering it
           		//communicate the balance of this account to the second request
            	if(!req.dest_account_number.equals("-1")){
	                String acc_number = String.valueOf(req.account_number);
	                int branch_number = Character.getNumericValue(acc_number.charAt(0));
	                for(int i = 0; i < branchAccounts[branch_number].size(); i++){
	                    if(branchAccounts[branch_number].get(i).get_account_number().equals(acc_number)){
	                    	double balance = branchAccounts[branch_number].get(i).get_balance();
	                        branchAccounts[branch_number].remove(i);
	                        int dest_branch_number = Integer.valueOf(req.dest_account_number);
	                        br_updater[dest_branch_number].notifyAccountTranfer(req.request_number, balance);
	                        break;
	                    }
	                }
	                //release lock associated with this branch
	                br_updater[branch_number].releaseBranchLock();
	            }
	            //if it is second request
	            //create a valid account having account number 1 more than
	            //the last account created in this branch
	            //and with the communicated amount as initial balance
	            //add this account to the linked list of specified branch number
	            else{
	            	double init_balance = req.transaction_amount;
	            	int branch_number = Integer.valueOf(req.account_number);
	                int temp = getAndIncrementNumAccounts(branch_number) + 1;
	                String acc_num = String.valueOf(temp);
	                while(acc_num.length() < 9){
	                    acc_num = "0" + acc_num;
	                }
	                acc_num = String.valueOf(branch_number) + acc_num;
	                branchAccounts[branch_number].add(new Account(acc_num, req.transaction_amount));
	                //release lock associated with this branch
	                br_updater[branch_number].releaseBranchLock();
	                //increment the value of finished tasks
	                counter.getAndIncrement();
	            }
            }

            int branch_number = Character.getNumericValue(req.account_number.charAt(0));
            //signal the termination of this thread to its corresponding branch
            br_updater[branch_number].signalEnd();

        }
    }
    
    //a class which contains all the necessary resources
    //required by the branch updaters (threads) for a single branch
    public static class branchUpdater {
    	//stores count of currently available threads in current branch
        private int available;
        //stores count of total threads in current branch
        private int total;
        //priority queue of requests corresponding to current branch
        PriorityQueue<Request> requestQueue = new PriorityQueue<Request>(10000000, new RequestComparator());
        //stores the money transfer request results corresponding to request id
        Map<Integer, Boolean> transferResult = new TreeMap<>();
        //stores status of account whose account number is same as key
        //true indicates busy
        //false indicates idle
        Map<String, Boolean> isBusy = new TreeMap<>();
        //stores the account transfer request results corresponding to request id
        //the balance corresponding to deleted account is stored
        Map<Integer, Double> accountTranferResult = new TreeMap<>();
        //stores if branch is undergoing an add/delete account operation
        Boolean isBranchLocked;
    	
    	//constructor
        public branchUpdater(int numThreads) {
            total = numThreads;
            available = total;
            isBranchLocked = false;
        }

        //checks if branch is locked
        //and acquires it if it isn't locked and "acquireBranchLockIfPossible" is 1
        synchronized public int checkBranchLock(int acquireBranchLockIfPossible){
        	if(acquireBranchLockIfPossible == 1){
        		if(isBranchLocked)
        			return 0;
        		else{
        			isBranchLocked = true;
        			return 1;
        		}
        	}
        	else{
        		if(isBranchLocked)
        			return 1;
        		else return 0;
        	}
        }

        //releases branch lock corresponding to current branch
        synchronized public void releaseBranchLock(){
        	isBranchLocked = false;
        }
    	
    	//function which is called when a thread terminates
        synchronized public void signalEnd() {
            this.available++;
            this.handleRequests();
        }

        //tries to acquire lock for account with account number as "acc_num"
        synchronized public int acquireLockIfPossible(String acc_num){
        	if(!isBusy.containsKey(acc_num)){
        		isBusy.put(acc_num, true);
        		return 1;
        	}
        	else{
        		if(!isBusy.get(acc_num)){
        			isBusy.put(acc_num, true);
        			return 1;
        		}
        		else return 0;
        	}
        }

        //releases lock corresponding to given account number
        synchronized public void releaseLock(String acc_num){
        	isBusy.put(acc_num, false);
        }
    	
    	//function for handling requests given to this branch
    	//a request is dispatched to a thread as soon as a thread
    	//is available and the request is ready for execution
        synchronized public void handleRequests(){
            while(!requestQueue.isEmpty()){   
                if(this.available > 0){
                    if(requestQueue.peek().type == 1 || requestQueue.peek().type == 2){
                    	if(checkBranchLock(0) == 0){
	                    	if(acquireLockIfPossible(requestQueue.peek().account_number) == 1){
		                        this.available--;
		                        Request headRequest = requestQueue.poll();
		                        Multithreading m = new Multithreading(headRequest);
		                        Thread newUpdater = new Thread(m);
		                        newUpdater.start();
		                    }
		                    else break;
		                }
		                else break;
                    }
                    else if(requestQueue.peek().type == 3){
                    	if(checkBranchLock(0) == 0){
	                    	if(!requestQueue.peek().dest_account_number.equals("-1")){
	                    		if(acquireLockIfPossible(requestQueue.peek().account_number) == 1){
			                        this.available--;
			                        Request headRequest = requestQueue.poll();
			                        Multithreading m = new Multithreading(headRequest);
			                        Thread newUpdater = new Thread(m);
			                        newUpdater.start();
			                    }
			                    else break;
	                    	}
	                    	else{
	                    		//if transfer money result is available in "transferResult"
	                    		if(transferResult.containsKey(requestQueue.peek().request_number)){
	                    			//if withdrawal was successful
	                    			if(transferResult.get(requestQueue.peek().request_number) == true){
	                    				if(acquireLockIfPossible(requestQueue.peek().account_number) == 1){
					                        this.available--;
					                        Request headRequest = requestQueue.poll();
					                        Multithreading m = new Multithreading(headRequest);
					                        Thread newUpdater = new Thread(m);
					                        newUpdater.start();
					                    }
					                    else break;
	                    			}
	                    			else{
	                    				Request headRequest = requestQueue.poll();
	                    				//increment the value of finished tasks
	                    				counter.getAndIncrement();
	                    			}
	                    		}
	                    		else break;
	                    	}
	                    }
	                    else break;
                    }
                    else if(requestQueue.peek().type == 4){
                    	if(checkBranchLock(1) == 1){
	                    	this.available--;
	                        Request headRequest = requestQueue.poll();
	                        Multithreading m = new Multithreading(headRequest);
	                        Thread newUpdater = new Thread(m);
	                        newUpdater.start();
	                    }
	                    else break;
                    }
                    else if(requestQueue.peek().type == 5){
                    	//waits until no thread is working on current branch
                    	if(available == total){
                    		if(checkBranchLock(1) == 1){
	                    		this.available--;
		                        Request headRequest = requestQueue.poll();
		                        Multithreading m = new Multithreading(headRequest);
		                        Thread newUpdater = new Thread(m);
		                        newUpdater.start();
		                    }
		                    else break;
                    	}
                    	else break;
                    }
                    else{
                    	if(!requestQueue.peek().dest_account_number.equals("-1")){
                    		if(available == total){
                    			if(checkBranchLock(1) == 1){
			                        this.available--;
			                        Request headRequest = requestQueue.poll();
			                        Multithreading m = new Multithreading(headRequest);
			                        Thread newUpdater = new Thread(m);
			                        newUpdater.start();
			                    }
			                    else break;
		                    }
		                    else break;
                    	}
                    	else{
                    		//if transfer account result is available in "accountTranferResult"
                    		//the initial amount for the new account is available there
                    		if(accountTranferResult.containsKey(requestQueue.peek().request_number)){
                    			if(checkBranchLock(1) == 1){
	                    			this.available--;
			                        Request headRequest = requestQueue.poll();
			                        headRequest.transaction_amount = accountTranferResult.get(headRequest.request_number);
			                        Multithreading m = new Multithreading(headRequest);
			                        Thread newUpdater = new Thread(m);
			                        newUpdater.start();
			                    }
			                    else break;
                    		}
                    		else break;
                    	}
                    }
                }
                else break;
            }
        }
    	
    	//add the given request to this branch
        synchronized public void add(Request r){
            this.requestQueue.add(r);
            this.handleRequests();
        }

        //notify money transfer result to this branch
        synchronized public void notifyTransactionOutcome(int id, boolean result){
        	transferResult.put(id, result);
        	this.handleRequests();
        }

        //notify account transfer result to this branch
        synchronized public void notifyAccountTranfer(int id, double balance){
        	accountTranferResult.put(id, balance);
        	this.handleRequests();
        }
    }
    
    //comparator for priority queue of requests
    static class RequestComparator implements Comparator<Request>{ 
        //Overriding compare() method of Comparator 
        public int compare(Request r1, Request r2) { 
            if (r1.request_number > r2.request_number) 
                return 1; 
            else if(r1.request_number < r2.request_number)
            	return -1;
            else{
                if(!r1.dest_account_number.equals("-1")){
                    return -1;
                }
                else{
                    return 1;
                }
            }
        } 
    } 
    
    //a class named Account which stores all the data members
    //and methods required for manipulating accounts
    public static class Account {
    	//stores the account number
        private String account_number;
        //stores the account balance
        private double balance;
    	
    	//constructor
        public Account(String acc_num, double init_balance) {
            account_number = acc_num;
            balance = init_balance;
        }
    	
    	//modify account
        synchronized public int modify(Request r){
            //deposit
            if(r.type == 1){
                this.balance += r.transaction_amount;
                return 1;
            }
            //withdraw
            else if(r.type == 2){
                if(Double.compare(this.balance, r.transaction_amount) >= 0) {
                    this.balance -= r.transaction_amount;
                    return 1;
                }
                else return 0;
            }
            //transfer money
            else{
            	//withdraw successful
                if(Double.compare(this.balance, r.transaction_amount) >= 0) {
                    this.balance -= r.transaction_amount;
                    return 1;
                }
                //withdraw failed
                else return 0;
            }
        }
        //returns account number of current account
        String get_account_number(){
            return this.account_number;
        }
        //returns account balance of current account
        double get_balance(){
        	return this.balance;
        }
    }

    //make sure account number has 10 digits
    public static String makeStringAsAccountNumber(String acc_num){
    	while(acc_num.length() < 10){
    		acc_num = "0" + acc_num;
    	}
    	return acc_num;
    }

    public static void main(String[] args) throws IOException, InterruptedException{
    	//input file
        String file = "testcase.txt";
        Scanner scanner = new Scanner(new File(file));
        //setting delimiters for scanner object
        scanner.useDelimiter(" |\\n|\\r\\n");
        
        //queue of request objects made from input
        Queue<Request> inputQueue = new LinkedList<Request>();
        //queue of request types in input
        Queue<Integer> typeQueue = new LinkedList<Integer>();

        System.out.println("Reading input file begins");

        //stores number of requests in file
        int n = scanner.nextInt();

        //iterating over all requests
        for(int tc = 1; tc <= n; tc++) {
        	//request type
            int t = scanner.nextInt();
            //add this to "typeQueue"
            typeQueue.add(t);
            //for every request type, take in the required arguments
            //make request objects
            //insert the objects in "inputQueue"
            if(t == 1 || t == 2){
                long acc_num = scanner.nextLong();
                String acc_num_string = makeStringAsAccountNumber(String.valueOf(acc_num));
                int branch_num = Character.getNumericValue(acc_num_string.charAt(0));
                double amount = scanner.nextDouble();
                Request r = new Request(tc, acc_num_string, "-1", t, amount);
                inputQueue.add(r);
                //br_updater[branch_num].add(r);
            }
            else if(t == 3){
                long source_acc_num = scanner.nextLong();
                String source_acc_num_string = makeStringAsAccountNumber(String.valueOf(source_acc_num));
                int source_branch_num = Character.getNumericValue(source_acc_num_string.charAt(0));
                long dest_acc_num = scanner.nextLong();
                String dest_acc_num_string = makeStringAsAccountNumber(String.valueOf(dest_acc_num));
                int dest_branch_num = Character.getNumericValue(dest_acc_num_string.charAt(0));
                double amount = scanner.nextDouble();
                Request r1 = new Request(tc, source_acc_num_string, dest_acc_num_string, t, amount);
                inputQueue.add(r1);
                //br_updater[source_branch_num].add(r1);
                Request r2 = new Request(tc, dest_acc_num_string, "-1", t, amount);
                inputQueue.add(r2);
                //br_updater[dest_branch_num].add(r2);
            }
            else if(t == 4){
                int branch_number = scanner.nextInt();
                double amount = scanner.nextDouble();
                Request r = new Request(tc, String.valueOf(branch_number), "-1", t, amount);
                inputQueue.add(r);
                //br_updater[branch_number].add(r);
            }
            else  if(t == 5){
                long acc_num = scanner.nextLong();
                String acc_num_string = makeStringAsAccountNumber(String.valueOf(acc_num));
                int branch_num = Character.getNumericValue(acc_num_string.charAt(0));
                Request r = new Request(tc, makeStringAsAccountNumber(String.valueOf(acc_num)), "-1", t, 0);
                inputQueue.add(r);
                //br_updater[branch_num].add(r);
            }
            else{
                long acc_num = scanner.nextLong();
                String acc_num_string = makeStringAsAccountNumber(String.valueOf(acc_num));
                int branch_num = Character.getNumericValue(acc_num_string.charAt(0));
                int dest_branch_number = scanner.nextInt();
                Request r1 = new Request(tc, makeStringAsAccountNumber(String.valueOf(acc_num)), String.valueOf(dest_branch_number), t, 0);
                inputQueue.add(r1);
                //br_updater[branch_num].add(r1);
                Request r2 = new Request(tc, String.valueOf(dest_branch_number), "-1", t, 0);
                inputQueue.add(r2);
                //br_updater[dest_branch_number].add(r2);
            }
        }
        //closing scanner object
        scanner.close();

        System.out.println("Reading input completed");

        System.out.println("Execution begins");

        //storing start time of execution
        long startTime = System.nanoTime();
        
        //iterating over all requests
        for(int tc = 1; tc <= n; tc++) {
        	//the type of request is the front element of "typeQueue"
            int t = typeQueue.poll();

            //for every request type, extract the associated
            //request objects from "inputQueue"
            if(t == 1 || t == 2){
                Request r = inputQueue.poll();
                int branch_num = Character.getNumericValue(r.account_number.charAt(0));
                br_updater[branch_num].add(r);
            }
            else if(t == 3){
                Request r1 = inputQueue.poll();
                int source_branch_num = Character.getNumericValue(r1.account_number.charAt(0));
                br_updater[source_branch_num].add(r1);
                Request r2 = inputQueue.poll();
                int dest_branch_num = Character.getNumericValue(r2.account_number.charAt(0));
                br_updater[dest_branch_num].add(r2);
            }
            else if(t == 4){
                Request r = inputQueue.poll();
                int branch_number = Character.getNumericValue(r.account_number.charAt(0));
                br_updater[branch_number].add(r);
            }
            else  if(t == 5){
                Request r = inputQueue.poll();
                int branch_num = Character.getNumericValue(r.account_number.charAt(0));
                br_updater[branch_num].add(r);
            }
            else{
                Request r1 = inputQueue.poll();
                int branch_num = Character.getNumericValue(r1.account_number.charAt(0));
                br_updater[branch_num].add(r1);
                Request r2 = inputQueue.poll();
                int dest_branch_number = Character.getNumericValue(r2.account_number.charAt(0));
                br_updater[dest_branch_number].add(r2);
            }
        }

        //wait until all the requests have been completed
        while(counter.get() != n){
        	Thread.sleep(100);
        }

        System.out.println("Execution ends");

        //storing end time of execution
        long endTime = System.nanoTime();
        //computing execution time
        long timeElapsed = endTime - startTime;
        System.out.println("Execution time in seconds : " + timeElapsed / 1000000000);
        
        //printAccountDetails();

    }

}
