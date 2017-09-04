import java.io.*;
import java.util.*;

enum Pivot {
    FIRST, LAST, MEDIAN
}

public class Solution {

    private static int qsortAndCount(List<Short> array, Pivot where) {

        if (array.size() <= 1) {
            return 0;
        }
        else {
            short i=1;

            switch(where) {
                case FIRST:
                    break;
                case LAST:
                    swap(array, 0, array.size()-1);
                    break;
                case MEDIAN:
                    swap(array, 0, getMedianIndex(array));
                    break;
            }

            short pivotNo = array.get(0);
            for (int j = 1; j < array.size(); j++) {
                if (array.get(j) < pivotNo) {
                    swap(array, i, j);
                    i++;
                }
            }

            swap(array, 0, i-1);

            return array.size() - 1
                    + qsortAndCount(array.subList(0, i-1), where)
                    + qsortAndCount(array.subList(i, array.size()), where);
        }
    }

    private static void swap(List<Short> list, int a, int b) {
        Short tmp = list.get(a);
        list.set(a, list.get(b));
        list.set(b, tmp);
    }

    private static int getMedianIndex(List<Short> list) {
        int first = 0, mid = (list.size()-1)/2, last = list.size()-1;
        short[] values = {list.get(first), list.get(mid), list.get(last)};
        Arrays.sort(values);

        int median = values[1];

        if (list.get(first) == median) {
            return first;
        }
        else if(list.get(mid) == median) {
            return mid;
        }
        else {
            return last;
        }
    }

    public static void main(String[] args) throws FileNotFoundException {

        Scanner input = new Scanner(new File("QuickSort.txt"));

        List<Short> array1 = new ArrayList<Short>(10000);
        List<Short> array2 = new ArrayList<Short>(10000);
        List<Short> array3 = new ArrayList<Short>(10000);

        int[] comparisons = {0, 0, 0};

        while(input.hasNextShort()) {
            short nextShort = input.nextShort();
            array1.add(nextShort);
            array2.add(nextShort);
            array3.add(nextShort);
        }

        comparisons[0] = qsortAndCount(array1, Pivot.FIRST);
        comparisons[1] = qsortAndCount(array2, Pivot.LAST);
        comparisons[2] = qsortAndCount(array3, Pivot.MEDIAN);

        System.out.println("Pivot chosen as first element: " + comparisons[0] + " comparisons.");
        System.out.println("Pivot chosen as last element: " + comparisons[1] + " comparisons.");
        System.out.println("Pivot chosen as median element: " + comparisons[2] + " comparisons.");

        input.close();
        return;
    }

}