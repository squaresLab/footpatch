import com.github.javaparser.ast.*;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.type.*;
import com.github.javaparser.ast.visitor.*;
import com.github.javaparser.ast.stmt.*;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.*;
import java.io.*;
import java.lang.String;
import java.util.Arrays;

public class Rename {

    static String term = "";
    static String termReplacement = "";

    /**
     * Simple visitor implementation for visiting MethodDeclaration nodes.
     */
    private static class GenericVisitor extends VoidVisitorAdapter<Void> {

        /**
         * Matches n in "if (n == null) {}"
         */
        @Override
        public void visit(NameExpr n, Void arg) {
          //System.out.println("term is: " + term);
          if (n.getName().toString().equals(term)) {
            n.setName(termReplacement);
          } else if (term.equals("")) {
            n.setName(termReplacement); // if it's empty (we couldn't extract it with infer) just assume there's one pvar and replace the first
          }
          super.visit(n,arg);
        }

        /**
         * Matches n in "Node n = new Node();"
         */
        @Override
        public void visit(SimpleName n, Void arg) {
          //System.out.println("term2 is: " + term);
          if (n.getIdentifier().toString().equals(term)) {
            n.setIdentifier(termReplacement);
          //} else if (term.equals("")) {
          //  n.setIdentifier(termReplacement);
          }
          super.visit(n,arg);
        }
    }

    public static String strJoin(String[] aArr, String sSep) {
          StringBuilder sbStr = new StringBuilder();
              for (int i = 0, il = aArr.length; i < il; i++) {
                        if (i > 0)
                                      sbStr.append(sSep);
                                sbStr.append(aArr[i]);
                                    }
                  return sbStr.toString();
    }

    public static void main(String[] args) {

      //System.out.println("args 0 is exactly: \"" + args[0] + "\"");

      String s = args[0];

      s = s.replaceAll("\\\\[n]", "\n"); // four slashes to escape backslash. then an n. but that last slash should not bind to n
      s = s.replaceAll("\\\\[r]", "");   // nuke \r

      //System.out.println("s is exactly: \"" + s + "\"");

      try {
      BlockStmt stmt = JavaParser.parseBlock(
            "{" +
            s +
            "}"
            );

      term = args[1];
      termReplacement = args[2];

      new GenericVisitor().visit(stmt,null);

      String lines[] = stmt.toString().split("\n");
      String deleted_lines[] = Arrays.copyOfRange(lines, 1, lines.length-1);
      String deleted_parenths = strJoin(deleted_lines, "");

      System.out.println(deleted_parenths);
      } catch (Exception e) {
        System.exit(1);
      }

    }
}
