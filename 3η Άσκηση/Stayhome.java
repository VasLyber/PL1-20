import java.io.BufferedReader;
import java.io.FileReader;
import java.awt.Point;
import java.util.*;
import java.lang.*;

public class Stayhome {


    public static void main(String[] args) throws Exception {

        //int beforeUsedMem=Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory();

        FileReader fileReader = new FileReader(args[0]);
        BufferedReader bufferedReader = new BufferedReader(fileReader);
        List<char[]> lines = new ArrayList<char[]>();
        String line = null;
        int N = 0;
        int M = 0;
        while ((line = bufferedReader.readLine()) != null) {
            if (N == 0) {
                M = (int) line.length();
            }
            lines.add(line.toCharArray());
            N++;
        }
        bufferedReader.close();
        // Telos diavasmata

        char[][] grid = lines.toArray(new char[lines.size()][]);
        HashMap<Point,  Integer> levelsHashMap = new HashMap<Point,  Integer>();
        LinkedList<Point> frontier = new LinkedList<Point>();
        Point Tsiord = new Point(-1, -1);
        int zero = 0;
        Point Home = new Point(-2, -2);
        LinkedList<Point> airport = new LinkedList<Point>();

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < M; j++) {
                if (grid[i][j] == 'W') {
                    frontier.add(new Point(i, j));
                    levelsHashMap.put(new Point(i, j), zero );
                }
                if (grid[i][j] == 'S') {
                    Tsiord = new Point(i, j);
                }
                if (grid[i][j] == 'T') {
                    Home = new Point(i, j);
                }
                if (grid[i][j] == 'A') {
                    airport.add(new Point(i, j));
                }
            }
        }
        LinkedList<Point> next = new LinkedList<Point>();
        LinkedList<Point> next1 = new LinkedList<Point>();

        int k = 1;
        int flag = 0;
        int air = 0;
        int flag2 = 0;
        LinkedList<Point> frontier1 = new LinkedList<Point>();

        while ((!frontier.isEmpty()) || (!frontier1.isEmpty())) {
            next.removeAll(next);
            next.removeAll(next1);
            if(k%2==0){
                if(flag==1 &&(!frontier.isEmpty())){
                    air++;
                }
                if(air == 6 && (!frontier.isEmpty())){
                  while (!airport.isEmpty()){
                    Point t = airport.remove();
                    levelsHashMap.put(t, k);
                    next1.add(t);
                  }
                  frontier1.addAll(next1);
                  flag = 0;
                  air = 0;
                  flag2 = 1;
                }
            while((!frontier.isEmpty())) {
                Point p = frontier.remove();
                int u = (int) p.getX();
                int v = (int) p.getY();
                // down
                if (u < N - 1) {
                    Point l = new Point(u + 1, v);
                    if (!levelsHashMap.containsKey(l) && (grid[u + 1][v] != 'X')) {
                      if(airport.contains(l) && flag==0){
                        flag=1;
                        airport.remove(l);
                      }
                      if(airport.contains(l) && flag==1){
                        airport.remove(l);
                      }
                        levelsHashMap.put(l, k);
                        next.add(l);
                    }
                }
                // left
                if (v > 0) {
                    Point m = new Point(u, v - 1);
                    if (!levelsHashMap.containsKey(m) && (grid[u][v - 1] != 'X')) {
                        if(airport.contains(m) && flag==0){
                          flag=1;
                          airport.remove(m);
                        }
                        if(airport.contains(m) && flag==1){
                          airport.remove(m);
                        }
                        levelsHashMap.put(m, k);
                        next.add(m);
                    }
                }
                // right
                if (v < M - 1) {
                    Point n = new Point(u, v + 1);
                    if (!levelsHashMap.containsKey(n) && (grid[u][v + 1] != 'X')) {
                        if(airport.contains(n) && flag==0){
                          flag=1;
                          airport.remove(n);
                        }
                        if(airport.contains(n) && flag==1){
                          airport.remove(n);
                        }
                        levelsHashMap.put(n, k);
                        next.add(n);
                    }
                }
                // up
                if (u > 0) {
                    Point o = new Point(u - 1, v);
                    if (!levelsHashMap.containsKey(o) && (grid[u - 1][v] != 'X')) {
                        if(airport.contains(o) && flag==0){
                          flag=1;
                          airport.remove(o);
                        }
                        if(airport.contains(o) && flag==1){
                          airport.remove(o);
                        }
                        levelsHashMap.put(o, k);
                        next.add(o);
                    }
                }

            }
            frontier.addAll(next);
            if((!frontier.isEmpty())){
              k++;
              continue;
            }
            if(flag==1){
              k = k + 5 - air;
              while (!airport.isEmpty()){
                Point z = airport.remove();
                levelsHashMap.put(z, k);
                next.add(z);
              }
              flag = 0;
              flag2 = 1;
              frontier1.addAll(next);
              k++;
              continue;
            }
            k++;
            continue;
        }
        if(k%2==1){
          if(flag2==1){
            while (!frontier1.isEmpty()) {
                Point p = frontier1.remove();
                int u = (int) p.getX();
                int v = (int) p.getY();
                // down
                if (u < N - 1) {
                    Point l = new Point(u + 1, v);
                    if (!levelsHashMap.containsKey(l) && (grid[u + 1][v] != 'X')) {
                        levelsHashMap.put(l, k);
                        next.add(l);
                    }
                }
                // left
                if (v > 0) {
                    Point m = new Point(u, v - 1);
                    if (!levelsHashMap.containsKey(m) && (grid[u][v - 1] != 'X')) {
                        levelsHashMap.put(m, k);
                        next.add(m);
                    }
                }
                // right
                if (v < M - 1) {
                    Point n = new Point(u, v + 1);
                    if (!levelsHashMap.containsKey(n) && (grid[u][v + 1] != 'X')) {
                        levelsHashMap.put(n, k);
                        next.add(n);
                    }
                }
                // up
                if (u > 0) {
                    Point o = new Point(u - 1, v);
                    if (!levelsHashMap.containsKey(o) && (grid[u - 1][v] != 'X')) {
                        levelsHashMap.put(o, k);
                        next.add(o);
                    }
                }

            }
            frontier1.addAll(next);
            k++;
          }
          else{
            if(flag==1){
                air++;
            }
            if(air == 6){
              while (!airport.isEmpty()){
                Point t = airport.remove();
                levelsHashMap.put(t, k);
                next1.add(t);
              }
              frontier1.addAll(next1);
              flag = 0;
              air = 0;
              flag2 = 1;
            }
            k++;
          }
        }
      }

      System.gc();

      HashMap<Point, Point> parent = new HashMap<Point, Point>();
      HashMap<Point,  Integer> level = new HashMap<Point,  Integer>();
      parent.put(Tsiord, new Point(-1, -1));
      level.put(Tsiord, 0);
      frontier.removeAll(frontier);
      frontier.add(Tsiord);
      int i = 1;


      while (!frontier.isEmpty()) {
          next.removeAll(next);
          while (!frontier.isEmpty()) {
              Point p = frontier.remove();
              int u = (int) p.getX();
              int v = (int) p.getY();
              Point Down = new Point(u + 1, v);
              Point Left = new Point(u, v - 1);
              Point Right = new Point(u, v + 1);
              Point Up = new Point(u - 1, v);

              if ((!level.containsKey(Down))&&(Down.getX() < N) && (grid[(int) Down.getX()][(int) Down.getY()] != 'X') &&
               (levelsHashMap.get(Down) > i)) {
                  level.put(Down, i);
                  parent.put(Down, p);
                  next.add(Down);
              }
              if (!level.containsKey(Left) && Left.getY() >= 0 && grid[(int) Left.getX()][(int) Left.getY()] != 'X'
                      && (levelsHashMap.get(Left) > i)) {
                  level.put(Left, i);
                  parent.put(Left, p);
                  next.add(Left);
              }
              if (!level.containsKey(Right) && Right.getY() < M && grid[(int) Right.getX()][(int) Right.getY()] != 'X'
                      && (levelsHashMap.get(Right) > i)) {
                  level.put(Right, i);
                  parent.put(Right, p);
                  next.add(Right);
              }
              if (!level.containsKey(Up) && Up.getX() >= 0 && grid[(int) Up.getX()][(int) Up.getY()] != 'X'
                      && (levelsHashMap.get(Up) > i)) {
                  level.put(Up, i);
                  parent.put(Up, p);
                  next.add(Up);
              }

          }
          frontier.addAll(next);
          i++;
      }
      System.gc();
      Point tr = new Point(-1, -1);
      String ans = " ";
      int b = 0;
      if(!parent.containsValue(parent.get(Home))){
          System.out.print("IMPOSSIBLE");
      }
      else{
      while (!parent.get(Home).equals(tr)) {
          if ((int) parent.get(Home).getX() == (int) Home.getX() - 1) {
              ans = 'D' + ans;
              Home = parent.get(Home);
              b++;
          } else if ((int) parent.get(Home).getY() == (int) Home.getY() + 1) {
              ans = 'L' + ans;
              Home = parent.get(Home);
              b++;
          } else if ((int) parent.get(Home).getY() == (int) Home.getY() - 1) {
              ans = 'R' + ans;
              Home = parent.get(Home);
              b++;
          } else if ((int) parent.get(Home).getX() == (int) Home.getX() + 1) {
              ans = 'U' + ans;
              Home = parent.get(Home);
              b++;
          }

      }
      System.out.println(b);
      System.out.print(ans);
    }
      System.gc();
    }


}
