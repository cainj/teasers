package hackerrank.thirtyDaysOfCode;

class Person {
    protected String firstName;
    protected String lastName;
    protected int idNumber;

    // Constructor
    Person(String firstName, String lastName, int identification){
        this.firstName = firstName;
        this.lastName = lastName;
        this.idNumber = identification;
    }

    // Print person data
    public void printPerson(){
        System.out.println(
                "Name: " + lastName + ", " + firstName
                        + 	"\nID: " + idNumber);
    }

}

class Student extends Person{
    private int[] testScores;

    /*
    *   Class Constructor
    *
    *   @param firstName - A string denoting the Person's first name.
    *   @param lastName - A string denoting the Person's last name.
    *   @param id - An integer denoting the Person's ID number.
    *   @param scores - An array of integers denoting the Person's test scores.
    */
    // Write your constructor here

    public Student(String firstName, String lastName, int id, int[] testScores){
        super(firstName, lastName, id);
        this.testScores = testScores;
    }
    /*
    *   Method Name: calculate
    *   @return A character denoting the grade.
    */
    // Write your method here
    public String calculate(){
        int sum = 0;
        for(int i = 0; i < testScores.length; i++){
            sum += testScores[i];
        }

        int average = sum / testScores.length;
        String grade = "";
        if( 90 <= average) grade = "O";
        else if(80 <= average) grade = "E";
        else if(70 <= average) grade = "A";
        else if(55 <= average) grade = "P";
        else if(40 <= average) grade = "D";
        else if(0 <= average) grade = "T";
        return grade;
    }
}