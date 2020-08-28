import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;
import java.lang.*;

public class Vaccine {
    public static List<Character> input_stack = new ArrayList<Character>();
    public static List<Character> input_stack_complement = new ArrayList<Character>();
    public static void main(String[] args) throws Exception {

        //read
        FileReader file = new FileReader(args[0]);
        BufferedReader buffer = new BufferedReader(file);
        List<String> lines = new ArrayList<String>();
        String line = null;
        String k = buffer.readLine();
        int N = Integer.parseInt(k);

        String input_string;
        String input_stack;
        String input_stack_complement;
        int input_stack_length;
        //char[] actions = {'c', 'p', 'r'};

        while ((line = buffer.readLine()) != null) {
            lines.add(line);
        }
        buffer.close();

        // Telos diavasmata

        for (int i = 0; i < N; i ++) {

            input_string = lines.get(i);
            input_stack = string_to_stack(input_string);
            input_stack_complement = complement(input_stack);
            input_stack_length = input_stack.length();

            State t = callback(input_stack, input_stack_complement, input_stack_length);

            System.out.println(t.getVaccine_string());

        }


    }

    private static String string_to_stack(String line){
        String stack = new String();

        for (int i = 0; i < line.length(); i++){
            stack += line.charAt(i);
        }

        return stack;

    }

    private static String complement(String stack) {
        String stack_complement = new String();
        for (int i = 0; i < stack.length(); i++){
            Character character = stack.charAt(i);

        // for (char character : chars) {
            if (character.equals('G')) {
                stack_complement += 'C';
            } else if (character.equals('C')) {
                stack_complement += 'G';
            } else if (character.equals('A')) {
                stack_complement += 'U';
            } else {
                stack_complement += 'A';
            }
        }

        return stack_complement;

    }

    private static State callback(String input_s, String input_s_c, int input_s_l) {
        // List<Character> empty = new ArrayList<Character>();
        State init = new State("","", false, false, false, 1, input_s, input_s_c, input_s_l);
        State init_p = init.accessible_initial();
        List<State> akses = new ArrayList<State>();

        Deque<State> Q = new ArrayDeque<State>(); //https://www.geeksforgeeks.org/deque-interface-java-example/

        //List<State> Q = new ArrayList<State>();
        Q.add(init_p);


        boolean secondary_flag = true;
        //boolean done = false;
        boolean append_flag;
        State s;
        State t;
        //State o;

        while (!(Q.isEmpty())) { //check null list

            s = Q.remove(); //163
            //List<State> internal_Q = new ArrayList<State>();
            Deque<State> internal_Q = new ArrayDeque<State>();
            if (secondary_flag) {
                secondary_flag = !secondary_flag; //thanks
                akses = s.accessible_secondary();
                //t init
                for (int k = 0; k < akses.size(); k++) {
                    t = akses.get(k);
                    if (t.check_action() == 2) {
                        return t;
                    } else if ( t.check_action() == 1) {
                        if (t.isUnchanged_after_push_flag()){
                            Q.add(t);
                            break;
                        } else {
                            internal_Q.add(t);
                        }
                    }
                }
                Q.addAll(internal_Q);
            } else {
                akses = s.accessible();
                for (int u = 0; u < akses.size(); u++) {
                    t = akses.get(u);
                    if (t.check_action() == 2) {
                        return t;
                    } else if ( t.check_action() == 1) {
                        if (t.isUnchanged_after_push_flag()) {
                            append_flag = true;
                            //for (int b = 0; b < Q.size(); b++) {
                            for (State o: Q){
                                if ( (o.isReverse_flag() == t.isReverse_flag()) && (o.isComplement_flag() == t.isComplement_flag()) && (o.getInput_stack_index() == t.getInput_stack_index()) && (o.getOut_stack() == t.getOut_stack()) ) {
                                    append_flag = false;
                                    break;
                                }
                            }
                            if (append_flag) {
                                Q.add(t);
                                break;
                            }
                        } else {
                            internal_Q.add(t);
                        }
                    }
                }
                Q.addAll(internal_Q);
            }




        }

        State errorpog = new State("error");

        return errorpog;

    }

}        
