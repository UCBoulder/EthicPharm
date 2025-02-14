import org.openscience.cdk.*;
import org.openscience.cdk.io.*;
import org.openscience.cdk.tools.manipulator.AtomContainerManipulator;
import org.openscience.cdk.tools.Aromaticity;
import org.openscience.cdk.tools.chemical.*;
import org.openscience.cdk.smiles.SmilesParser;
import org.openscience.cdk.smiles.SmilesGenerator;
import org.openscience.cdk.interfaces.IAtomContainer;
import org.openscience.cdk.io.ChemFileWriter;
import org.openscience.cdk.io.CSVWriter;

import java.io.*;
import java.util.*;
import java.util.List;

public class FunctionalGroupProcessor {

    public static void processCSV(String inputFilePath, String outputFilePath) {
        try {
            // Initialize functional group finder
            ErtlFunctionalGroupsFinder fgFinder = new ErtlFunctionalGroupsFinder();

            // Read the CSV file
            BufferedReader reader = new BufferedReader(new FileReader(inputFilePath));
            List<String[]> rows = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                rows.add(line.split(","));
            }
            reader.close();

            // Prepare output CSV file
            FileWriter writer = new FileWriter(outputFilePath);
            CSVWriter csvWriter = new CSVWriter(writer);
            String[] header = new String[]{"SMILES", "FunctionalGroup1", "FunctionalGroup2", /* add more headers for each functional group */};
            csvWriter.writeNext(header);

            SmilesParser smilesParser = new SmilesParser(SilentChemObjectBuilder.getInstance());
            SmilesGenerator smilesGenerator = new SmilesGenerator(SmiFlavor.Canonical | SmiFlavor.UseAromaticSymbols);

            for (String[] row : rows) {
                String smiles = row[0];
                IAtomContainer molecule = smilesParser.parseSmiles(smiles);
                AtomContainerManipulator.percieveAtomTypesAndConfigureAtoms(molecule);
                Aromaticity aromaticity = new Aromaticity(ElectronDonation.cdk(), Cycles.cdkAromaticSet());
                aromaticity.apply(molecule);

                List<IAtomContainer> functionalGroups = fgFinder.find(molecule);
                Map<String, Integer> fgPresence = new HashMap<>();

                for (IAtomContainer fg : functionalGroups) {
                    String fgSmiles = smilesGenerator.create(fg);
                    fgPresence.put(fgSmiles, 1);  // Mark presence of functional group
                }

                // Create output row
                List<String> outputRow = new ArrayList<>();
                outputRow.add(smiles);
                for (String headerName : header) {
                    outputRow.add(fgPresence.getOrDefault(headerName, 0).toString());
                }
                csvWriter.writeNext(outputRow.toArray(new String[0]));
            }

            csvWriter.close();
            System.out.println("Processing complete. Output saved to " + outputFilePath);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        String inputFilePath = "input_smiles.csv";  // Path to your input CSV file
        String outputFilePath = "output_functional_groups.csv";  // Path to your output CSV file
        processCSV(inputFilePath, outputFilePath);
    }
}
