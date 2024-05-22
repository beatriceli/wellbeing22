import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class App {

    public static void main(String[] args) throws Exception {
        // csv reader
        BufferedReader br = null;

        // read csv files that end in "locations.csv" in "/Users/beatriceli/Documents/PhD_Research/GitHub/well-being/lll_aware"
        // return the csv file
        try {
            File myDirectory = new File("/Users/beatriceli/Documents/PhD_Research/GitHub/well-being/lll_aware");
            String[] dirs = myDirectory.list();
            List<String> dirsList = Arrays.asList(dirs);
            List<String> pids = new ArrayList<>();
            
            for (String dir : dirsList) {
                if (dir.startsWith("aware")) {
                    pids.add(dir);
                }
            }

            // go through each pid and get the csv file that ends in "locations.csv"
            for (String pid : pids) {
                File myDirectory2 = new File("/Users/beatriceli/Documents/PhD_Research/GitHub/well-being/lll_aware/" + pid);
                String[] files = myDirectory2.list();
                List<String> filelist = Arrays.asList(files);
                List<String> locations = new ArrayList<>();
                
                for (String f : filelist) {
                    if (f.endsWith("locations.csv")) {
                        locations.add(f);
                    }
                }
                
                System.out.println("List of locations: " + locations);
            }

            System.out.println("List of pids: " + pids);

        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (br != null) {
                br.close();
                }
            } catch (IOException exception) {
                exception.printStackTrace();
            }
        
    }

    }

}
