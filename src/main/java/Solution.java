import java.util.*;

public class Solution {

    public static void  main(String[] args){
        List<List<String>> codeList = new ArrayList<List<String>>();
        List<String> shoppingCart = new ArrayList<String>();
        shoppingCart.add("apple");
        shoppingCart.add("apple");
        shoppingCart.add("banana");
        shoppingCart.add("apple");
        shoppingCart.add("banana");
        ArrayList<String> x = new ArrayList<String>();
        x.add("apple");
        x.add("apple");
        ArrayList<String> y = new ArrayList<String>();
        y.add("banana");
        y.add("anything");
        y.add("banana");
        codeList.add(x);
        codeList.add(y);
        System.out.println(checkWinner(codeList, shoppingCart));

    }

    // METHOD SIGNATURE BEGINS, THIS METHOD IS REQUIRED
    public static int checkWinner(List<List<String>> codeList,
                           List<String> shoppingCart)
    {
        String fruits = "";
        // WRITE YOUR CODE HERE
        for(int i = 0; i < codeList.size(); i++){
            List<String> x = codeList.get(i);
            for(int j = 0; j < x.size(); j++){
                fruits = fruits.concat(" " + x.get(j));
            }
        }
        String[] orderedFruits = (fruits.trim()).split(" ");

        int i = 0;
        while( i < shoppingCart.size()) {
            int j = 0;
            Boolean keepGoing = true;
            while(keepGoing) {
                if (orderedFruits[j].equalsIgnoreCase(shoppingCart.get(i)) || orderedFruits[j].equalsIgnoreCase( "anything")) {
                    if (j == codeList.size()) return 1;
                    j++;
                } else keepGoing = false;
                i++;
            }
        }
        return 0;
    }

}