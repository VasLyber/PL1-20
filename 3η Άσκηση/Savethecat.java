import java.io.BufferedReader;
import java.io.FileReader;
import java.awt.Point;
import java.util.*;
import java.lang.*;

public class Savethecat {
    

    public static void main(String[] args) throws Exception {
        
        //long beforeUsedMem=Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory();

        FileReader fileReader = new FileReader(args[0]);
        BufferedReader bufferedReader = new BufferedReader(fileReader);
        List<char[]> lines = new ArrayList<char[]>();
        String line = null;
        long N = 0;
        long M = 0;
        while ((line = bufferedReader.readLine()) != null) {
            if (N == 0) {
                M = (long) line.length();
            }
            lines.add(line.toCharArray());
            N++;
        }
        bufferedReader.close();
        // Telos diavasmata

        char[][] grid = lines.toArray(new char[lines.size()][]);
        HashMap<Point,  Long> levelsHashMap = new HashMap<Point,  Long>();
        LinkedList<Point> frontier = new LinkedList<Point>();
        Point cat = new Point(-1, -1);
        Long zero = 0;
        
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < M; j++) {
                if (grid[i][j] == 'W') {
                    frontier.add(new Point(i, j));
                    levelsHashMap.put(new Point(i, j), zero );
                }
                if (grid[i][j] == 'A') {
                    cat = new Point(i, j);
                }
            }
        }
        LinkedList<Point> next = new LinkedList<Point>();
        long k = 1;
        while (!frontier.isEmpty()) {
            next.removeAll(next);
            while (!frontier.isEmpty()) {
                Point p = frontier.remove();
                long u = (long) p.getX();
                long v = (long) p.getY();
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
                    }import java.io.BufferedReader;
import java.io.FileReader;
import java.awt.Point;
import java.util.*;
import java.lang.*;

public class Savethecat {
    

    public static void main(String[] args) throws Exception {
        
        //long beforeUsedMem=Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory();

        FileReader fileReader = new FileReader(args[0]);
        BufferedReader bufferedReader = new BufferedReader(fileReader);
        List<char[]> lines = new ArrayList<char[]>();
        String line = null;
        long N = 0;
        long M = 0;
        while ((line = bufferedReader.readLine()) != null) {
            if (N == 0) {
                M = (long) line.length();
            }
            lines.add(line.toCharArray());
            N++;
        }
        bufferedReader.close();
        // Telos diavasmata

        char[][] grid = lines.toArray(new char[lines.size()][]);
        HashMap<Point,  Long> levelsHashMap = new HashMap<Point,  Long>();
        LinkedList<Point> frontier = new LinkedList<Point>();
        Point cat = new Point(-1, -1);
        Long zero = 0;
        
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < M; j++) {
                if (grid[i][j] == 'W') {
                    frontier.add(new Point(i, j));
                    levelsHashMap.put(new Point(i, j), zero );
                }
                if (grid[i][j] == 'A') {
                    cat = new Point(i, j);
                }
            }
        }
        LinkedList<Point> next = new LinkedList<Point>();
        long k = 1;
        while (!frontier.isEmpty()) {
            next.removeAll(next);
            while (!frontier.isEmpty()) {
                Point p = frontier.remove();
                long u = (long) p.getX();
                long v = (long) p.getY();
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
            frontier.addAll(next);
            k++;
        }
        System.gc();

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < M; j++) {
                Point p = new Point(i, j);
                if (!levelsHashMap.containsKey(p) && grid[i][j] != 'X') {
                    levelsHashMap.put(p,  Long.MIN_VALUE);
                }
            }
        }
        System.gc();

        HashMap<Point, Point> parent = new HashMap<Point, Point>();
        HashMap<Point,  Long> level = new HashMap<Point,  Long>();
        parent.put(cat, new Point(-1, -1));
        level.put(cat, 0);
        long maxTime = -1;
        Point Target = new Point(-1, -1);
        frontier.removeAll(frontier);
        frontier.add(cat);
        long i = 1;

        while (!frontier.isEmpty()) {
            next.removeAll(next);
            while (!frontier.isEmpty()) {
                Point p = frontier.remove();
                long u = (long) p.getX();
                long v = (long) p.getY();
                if (levelsHashMap.get(p) ==  Long.MIN_VALUE) {
                    maxTime =  Long.MIN_VALUE;
                    Target = p;
                } else if (levelsHashMap.get(p) > maxTime + 1) {
                    maxTime = levelsHashMap.get(p) - 1;
                    Target = p;
                }

                Point Down = new Point(u + 1, v);
                Point Left = new Point(u, v - 1);
                Point Right = new Point(u, v + 1);
                Point Up = new Point(u - 1, v);

                if (!level.containsKey(Down) && Down.getX() < N && grid[(long) Down.getX()][(long) Down.getY()] != 'X'
                        && (levelsHashMap.get(Down) ==  Long.MIN_VALUE || levelsHashMap.get(Down) > i)
                        && maxTime !=  Long.MIN_VALUE) {
                    level.put(Down, i);
                    parent.put(Down, p);
                    next.add(Down);
                }
                if (!level.containsKey(Left) && Left.getY() >= 0 && grid[(long) Left.getX()][(long) Left.getY()] != 'X'
                        && (levelsHashMap.get(Left) ==  Long.MIN_VALUE || levelsHashMap.get(Left) > i)) {
                    level.put(Left, i);
                    parent.put(Left, p);
                    next.add(Left);
                }
                if (!level.containsKey(Right) && Right.getY() < M && grid[(long) Right.getX()][(long) Right.getY()] != 'X'
                        && (levelsHashMap.get(Right) ==  Long.MIN_VALUE || levelsHashMap.get(Right) > i)
                        && maxTime !=  Long.MIN_VALUE) {
                    level.put(Right, i);
                    parent.put(Right, p);
                    next.add(Right);
                }
                if (!level.containsKey(Up) && Up.getX() >= 0 && grid[(long) Up.getX()][(long) Up.getY()] != 'X'
                        && (levelsHashMap.get(Up) ==  Long.MIN_VALUE || levelsHashMap.get(Up) > i)) {
                    level.put(Up, i);
                    parent.put(Up, p);
                    next.add(Up);
                }

            }
            frontier.addAll(next);
            i++;
        }
        System.gc();

        if (maxTime ==  Long.MIN_VALUE)
            System.out.println("infinity");
        else
            System.out.println(maxTime);

        Point tr = new Point(-1, -1);

        if (Target == cat)
            System.out.println("stay");
        else {
            String ans = " ";
            while (!parent.get(Target).equals(tr)) {
                if ((long) parent.get(Target).getX() == (long) Target.getX() - 1) {
                    ans = 'D' + ans;
                    Target = parent.get(Target);
                } else if ((long) parent.get(Target).getY() == (long) Target.getY() + 1) {
                    ans = 'L' + ans;
                    Target = parent.get(Target);
                } else if ((long) parent.get(Target).getY() == (long) Target.getY() - 1) {
                    ans = 'R' + ans;
                    Target = parent.get(Target);
                } else if ((long) parent.get(Target).getX() == (long) Target.getX() + 1) {
                    ans = 'U' + ans;
                    Target = parent.get(Target);
                }
            }
            System.out.println(ans);
            System.gc();
        }
       // long afterUsedMem=Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory();
       // long actualMemUsed=afterUsedMem-beforeUsedMem;
       // System.out.println(actualMemUsed);
    }


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
            frontier.addAll(next);
            k++;
        }
        System.gc();

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < M; j++) {
                Point p = new Point(i, j);
                if (!levelsHashMap.containsKey(p) && grid[i][j] != 'X') {
                    levelsHashMap.put(p,  Long.MIN_VALUE);
                }
            }
        }
        System.gc();

        HashMap<Point, Point> parent = new HashMap<Point, Point>();
        HashMap<Point,  Long> level = new HashMap<Point,  Long>();
        parent.put(cat, new Point(-1, -1));
        level.put(cat, 0);
        long maxTime = -1;
        Point Target = new Point(-1, -1);
        frontier.removeAll(frontier);
        frontier.add(cat);
        long i = 1;

        while (!frontier.isEmpty()) {
            next.removeAll(next);
            while (!frontier.isEmpty()) {
                Point p = frontier.remove();
                long u = (long) p.getX();
                long v = (long) p.getY();
                if (levelsHashMap.get(p) ==  Long.MIN_VALUE) {
                    maxTime =  Long.MIN_VALUE;
                    Target = p;
                } else if (levelsHashMap.get(p) > maxTime + 1) {
                    maxTime = levelsHashMap.get(p) - 1;
                    Target = p;
                }

                Point Down = new Point(u + 1, v);
                Point Left = new Point(u, v - 1);
                Point Right = new Point(u, v + 1);
                Point Up = new Point(u - 1, v);

                if (!level.containsKey(Down) && Down.getX() < N && grid[(long) Down.getX()][(long) Down.getY()] != 'X'
                        && (levelsHashMap.get(Down) ==  Long.MIN_VALUE || levelsHashMap.get(Down) > i)
                        && maxTime !=  Long.MIN_VALUE) {
                    level.put(Down, i);
                    parent.put(Down, p);
                    next.add(Down);
                }
                if (!level.containsKey(Left) && Left.getY() >= 0 && grid[(long) Left.getX()][(long) Left.getY()] != 'X'
                        && (levelsHashMap.get(Left) ==  Long.MIN_VALUE || levelsHashMap.get(Left) > i)) {
                    level.put(Left, i);
                    parent.put(Left, p);
                    next.add(Left);
                }
                if (!level.containsKey(Right) && Right.getY() < M && grid[(long) Right.getX()][(long) Right.getY()] != 'X'
                        && (levelsHashMap.get(Right) ==  Long.MIN_VALUE || levelsHashMap.get(Right) > i)
                        && maxTime !=  Long.MIN_VALUE) {
                    level.put(Right, i);
                    parent.put(Right, p);
                    next.add(Right);
                }
                if (!level.containsKey(Up) && Up.getX() >= 0 && grid[(long) Up.getX()][(long) Up.getY()] != 'X'
                        && (levelsHashMap.get(Up) ==  Long.MIN_VALUE || levelsHashMap.get(Up) > i)) {
                    level.put(Up, i);
                    parent.put(Up, p);
                    next.add(Up);
                }

            }
            frontier.addAll(next);
            i++;
        }
        System.gc();

        if (maxTime ==  Long.MIN_VALUE)
            System.out.println("infinity");
        else
            System.out.println(maxTime);

        Point tr = new Point(-1, -1);

        if (Target == cat)
            System.out.println("stay");
        else {
            String ans = " ";
            while (!parent.get(Target).equals(tr)) {
                if ((long) parent.get(Target).getX() == (long) Target.getX() - 1) {
                    ans = 'D' + ans;
                    Target = parent.get(Target);
                } else if ((long) parent.get(Target).getY() == (long) Target.getY() + 1) {
                    ans = 'L' + ans;
                    Target = parent.get(Target);
                } else if ((long) parent.get(Target).getY() == (long) Target.getY() - 1) {
                    ans = 'R' + ans;
                    Target = parent.get(Target);
                } else if ((long) parent.get(Target).getX() == (long) Target.getX() + 1) {
                    ans = 'U' + ans;
                    Target = parent.get(Target);
                }
            }
            System.out.println(ans);
            System.gc();
        }
       // long afterUsedMem=Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory();
       // long actualMemUsed=afterUsedMem-beforeUsedMem;
       // System.out.println(actualMemUsed);
    }


}
