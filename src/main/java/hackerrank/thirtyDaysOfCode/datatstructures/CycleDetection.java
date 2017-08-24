package hackerrank.thirtyDaysOfCode.datatstructures;

/**
 * https://www.hackerrank.com/challenges/detect-whether-a-linked-list-contains-a-cycle
 */
public class CycleDetection {

    /*
Detect a cycle in a linked list. Note that the head pointer may be 'null' if the list is empty.

A Node is defined as: */

    class Node {
        int data;
        Node next;
    }

    boolean hasCycle(Node head) {
        if( head == null)
            return false;

        int rover = 1;
        Node trailer = new Node();
        trailer.data = head.data;
        trailer.next = head.next;
        while( head != null){
            if(rover != 1 && head == trailer && head.next != null)
                return true;
            if(rover % 3 == 0){
                trailer = trailer.next;
            }
            rover++;
            head = head.next;
        }
        return false;
    }
}
