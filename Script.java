import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

public class Script {

	public static void main(String[] args) {
		try {
			String line = null;
			String str = null;
			String[] splitted;

			File f1 = new File("C:\\Users\\Geghi\\Desktop\\Italy.txt");
			File f2 = new File("C:\\Users\\Geghi\\Desktop\\ItalyMonth.txt");

			FileReader fr = new FileReader(f1);
			BufferedReader br = new BufferedReader(fr);
			FileWriter fw = new FileWriter(f2);
			BufferedWriter out = new BufferedWriter(fw);
			line = br.readLine();
			while (line != null) {
				splitted = line.split("\",");
				//elimina tutte le virgolette e tutte le virgole.
				str = splitted[5].replaceAll("\"", "").replaceAll(",", "");
				System.out.println(str);
				out.write(str + "\n");
				line = br.readLine();
			}
			fr.close();
			br.close();
			out.flush();
			out.close();
			System.out.println("All working properly");
		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

}

