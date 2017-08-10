package hackerrank.thirtyDaysOfCode;

import java.util.Scanner;

class Node {
    int data;
    Node next;
    Node(int d) {
        data = d;
        next = null;
    }
}

class Solution {


    public static Node insert(Node head, int data) {
        //Complete this method
        if(head == null){
            Node node = new Node(data);
            node.next = head;
            return node;
        } else if(head.next == null){
            Node node = new Node(data);
            head.next = node;
            return head;
        }
        else {
            Node tail = head.next;
            while(tail.next != null) {
                tail = tail.next;
            }
            Node node = new Node(data);
            tail.next = node;
            return head;
        }
    }

    public static void display(Node head) {
        Node start = head;
        while(start != null) {
            start = start.next;
        }
    }

    public static void main(String args[]) {
        Scanner sc = new Scanner(System.in);
        Node head = null;
        int N = sc.nextInt();

        while(N-- > 0) {
            int ele = sc.nextInt();
            head = insert(head,ele);
        }
        display(head);
        sc.close();
    }

}