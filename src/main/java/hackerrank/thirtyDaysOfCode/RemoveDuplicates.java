package hackerrank.thirtyDaysOfCode;

import java.util.Scanner;

class RNode {
    int data;
    RNode next;
    RNode(int d) {
        data = d;
        next = null;
    }
}

/**
 * https://www.hackerrank.com/challenges/30-linked-list-deletion/problem
 */
class RemoveDuplicates {

    public static RNode insert(RNode head, int data)
    {
        RNode p = new RNode(data);
        if(head==null)
            head=p;
        else if(head.next==null)
            head.next=p;
        else
        {
            RNode start=head;
            while(start.next!=null)
                start=start.next;
            start.next=p;

        }
        return head;
    }

    public static RNode removeDuplicates(RNode head) {
        RNode prev = head;
        RNode tail = head.next;
        while (tail != null) {
            if (tail.data == prev.data) {
                prev.next = tail.next;
                tail = tail.next;
            } else {
                prev = tail;
                tail = prev.next;
            }
        }
        return head;
    }

    public static void display(RNode head)
    {
        RNode start=head;
        while(start!=null)
        {
            System.out.print(start.data+" ");
            start=start.next;
        }
    }
    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);
        RNode head = null;
        int T=sc.nextInt();
        while(T-->0){
            int ele=sc.nextInt();
            head=insert(head,ele);
        }
        head=removeDuplicates(head);
        display(head);

    }
}