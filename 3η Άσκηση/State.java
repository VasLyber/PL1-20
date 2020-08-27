import java.lang.*;
import java.util.*;

public class State {

    private List<Character> out_stack; //= new ArrayList<Character>();
    private String vaccine_string;
    private boolean complement_flag;
    private boolean reverse_flag;
    private boolean unchanged_after_push_flag;
    private int input_stack_index;
    private char[] actions = {'c', 'p', 'r'};
    private List<Character> input_stack;// = new ArrayList<Character>();
    private List<Character> input_stack_complement;// = new ArrayList<Character>();
    private int input_stack_length;


    public State(List<Character> out_stack, String vaccine_string, boolean complement_flag, boolean reverse_flag, boolean unchanged_after_push_flag, int input_stack_index, List<Character> input_stack, List<Character> input_stack_complement, int input_stack_length) {
        this.out_stack = out_stack;
        this.vaccine_string = vaccine_string;
        this.complement_flag = complement_flag;
        this.reverse_flag = reverse_flag;
        this.unchanged_after_push_flag = unchanged_after_push_flag;
        this.input_stack_index = input_stack_index; //mexri edw
        this.input_stack = input_stack;
        this.input_stack_complement = input_stack_complement;
        this.input_stack_length = input_stack_length;
    }

    public State(String vaccine_string){
        this.vaccine_string = vaccine_string;
    }

    public State deep_copy(){

        List<Character> out_stack_new = new ArrayList<>();
        String vaccine_string_new;
        boolean complement_flag_new;
        boolean reverse_flag_new;
        boolean unchanged_after_push_flag_new;
        int input_stack_index_new;

        for (int j = 0; j < this.out_stack.size(); j++) {
            out_stack_new.add(j, this.out_stack.get(j));
        }
        vaccine_string_new = this.vaccine_string;
        complement_flag_new = this.complement_flag;
        reverse_flag_new = this.reverse_flag;
        unchanged_after_push_flag_new = false;
        input_stack_index_new = this.input_stack_index;
        return new State(out_stack_new, vaccine_string_new, complement_flag_new, reverse_flag_new, unchanged_after_push_flag_new, input_stack_index_new, this.input_stack, this.input_stack_complement, this.input_stack_length);

    }

    public State deep_copy_reverse_complement(){

        List<Character> out_stack_new ;//= new ArrayList<Character>();
        String vaccine_string_new;
        boolean complement_flag_new;
        boolean reverse_flag_new;
        boolean unchanged_after_push_flag_new;
        int input_stack_index_new;

        out_stack_new = this.out_stack;
        vaccine_string_new = this.vaccine_string;
        complement_flag_new = this.complement_flag;
        reverse_flag_new = this.reverse_flag;
        unchanged_after_push_flag_new = false;
        input_stack_index_new = this.input_stack_index;
        return new State(out_stack_new, vaccine_string_new, complement_flag_new, reverse_flag_new, unchanged_after_push_flag_new, input_stack_index_new, this.input_stack, this.input_stack_complement, this.input_stack_length);

    }

    public void complement(){
        this.complement_flag = !this.complement_flag;
    }

    public void reverse(){
        this.reverse_flag = !this.reverse_flag;
    }

    
    public void push(){

        Character last_base;
        if (!this.complement_flag) {
            last_base = input_stack.get(input_stack.size() - this.input_stack_index);

        } else {
            last_base = input_stack_complement.get(input_stack_complement.size() - this.input_stack_index);
            
        }

        this.input_stack_index += 1;

        if (!this.reverse_flag) {
            if (this.out_stack.size() > 0) {
                
                if (this.out_stack.get(this.out_stack.size() - 1) != last_base) {
                    
                    this.out_stack.add(last_base);
                } else {
                    this.unchanged_after_push_flag = true;
                }
            } else {
                this.out_stack.add(last_base);
            }
        } else {
            if (this.out_stack.size() > 0 ) {
                
                if (this.out_stack.get(0) != last_base) {
                    this.out_stack.add(0, last_base);
                } else {
                    this.unchanged_after_push_flag = true;
                }
            } else {
                this.out_stack.add(0, last_base);
            }
        }

    }

    
    public void take_action(Character action){
        if (action.equals('c')){
            this.complement();
        } else if (action.equals('p')) {
            this.push();
        } else if (action.equals('r')) {
            this.reverse();
        }
        this.vaccine_string += action;
        //this.vaccine_string.concat(String.valueOf(action));
    }

    public State accessible_initial (){
        State temp = this.deep_copy();
        temp.take_action('p');
        return temp;
    }

    public List<State> accessible_secondary(){
        char[] actions_secondary = {'c', 'p'};
        List<State> acc_states = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            if (this.vaccine_string.length() > 0 ) {
                if ( (actions_secondary[i] == 'c' && (this.vaccine_string.charAt(this.vaccine_string.length()-1) == 'c') ) || (actions_secondary[i] == 'c' && (this.vaccine_string.charAt(this.vaccine_string.length()-1) == 'r') ) ){
                    continue;
                }
            }
            State temp = this.deep_copy();
            temp.take_action(actions_secondary[i]);
            acc_states.add(temp);
        }
        return acc_states;
    }


    public List<State> accessible (){

        List<State> acc_states = new ArrayList<>();

        for (int i = 0; i < 3; i++) {
            if (this.vaccine_string.length() > 0 ) {
                if ( (actions[i] == 'c' && (this.vaccine_string.charAt(this.vaccine_string.length()-1) == 'c')) || (actions[i] == 'r' && (this.vaccine_string.charAt(this.vaccine_string.length()-1) == 'r')) || (actions[i] == 'c' && (this.vaccine_string.charAt(this.vaccine_string.length()-1) == 'r') ) ){
                    continue;
                }
            }
            State temp;

            if (actions[i] == 'p') {
                temp = this.deep_copy();
            } else {
                temp = this.deep_copy_reverse_complement();
            }


            temp.take_action(actions[i]);
            acc_states.add(temp);
        }
        return acc_states;
    }

    
    public int check_action() {

        Set<Character> stackset = new HashSet<>(this.out_stack);

        int stacksize = this.out_stack.size();
        int setsize = stackset.size();

        if ( stacksize == setsize ) { //duplicheck
            if (input_stack_length == this.input_stack_index - 1) {
                return 2;
            }
            return 1;
        }

        return 0;

    }



    public boolean isUnchanged_after_push_flag() {
        return unchanged_after_push_flag;
    }

    public boolean isComplement_flag() {
        return complement_flag;
    }

    public boolean isReverse_flag() {
        return reverse_flag;
    }

    public List<Character> getOut_stack() {
        return out_stack;
    }

    public int getInput_stack_index() {
        return input_stack_index;
    }

    public String getVaccine_string() {
        return vaccine_string;
    }
}
