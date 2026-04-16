import os
import pandas as pd

# ---------------------------
# CONFIGURATION
# ---------------------------
ROOT_FOLDER = r"C:\Users\darwi\OneDrive\Dokumente\Ismael Darwich\Studium\Bachelorarbeit Ergebnisse\Evaluation CSV"

# Mapping for dataset detection
DATASET_MAP = {
    "rw": "Random Walk",
    "weo": "WEO",
    "ar1": "AR(1)",
    "arima1_1_0": "ARIMA(1,1,0)",
    "arima_auto": "ARIMA BIC",
    "oecd": "OECD"
}

# ---------------------------
# VARIATION DETECTION
# ---------------------------
def detect_variation(path_lower):
    has_fitted = "fitted_mean" in path_lower
    has_mean0 = "mean 0 assumption" in path_lower
    has_unbiased = "unbiased var est" in path_lower
    has_5000 = "5000samples" in path_lower

    if has_fitted and has_unbiased:
        return "fitted_mean & unbiased VAR"
    elif has_mean0 and has_unbiased:
        return "mean0 & unbiased VAR"
    elif has_fitted:
        return "fitted_mean"
    elif has_mean0:
        return "mean0"
    elif has_5000:
        return "5000"
    else:
        return ""  # no variation

# ---------------------------
# DATASET DETECTION
# ---------------------------
def detect_dataset(filename_lower):
    for key, name in DATASET_MAP.items():
        if key in filename_lower:
            return name
    return "unknown"

# ---------------------------
# MAIN PROCESS
# ---------------------------
def process_csv_files(root_folder):
    all_data = []

    for dirpath, dirnames, filenames in os.walk(root_folder):
        for file in filenames:
            if file.lower().endswith(".csv"):

                csv_path = os.path.join(dirpath, file)
                print(f"Processing: {csv_path}")

                # Read CSV
                try:
                    df = pd.read_csv(csv_path)
                except Exception:
                    df = pd.read_csv(csv_path, sep=";")

                # -----------------------------------
                # METHOD (first folder after root)
                # -----------------------------------
                relative_path = os.path.relpath(dirpath, root_folder)
                parts = relative_path.split(os.sep)
                method = parts[0] if len(parts) > 0 else "unknown"

                # -----------------------------------
                # VARIATION
                # -----------------------------------
                path_lower = dirpath.lower()
                variation = detect_variation(path_lower)

                # -----------------------------------
                # FREQUENCY
                # -----------------------------------
                frequency = "quarterly" if "quarterly" in path_lower else "annually"

                # -----------------------------------
                # DATASET
                # -----------------------------------
                dataset = detect_dataset(file.lower())

                # -----------------------------------
                # ADD COLUMNS
                # -----------------------------------
                df.insert(0, "method", method)
                df.insert(1, "variation", variation)
                df.insert(2, "dataset", dataset)
                df["frequency"] = frequency

                # Save individual Excel
                excel_path = csv_path.replace(".csv", ".xlsx")
                df.to_excel(excel_path, index=False)
                print(f"Created: {excel_path}")

                all_data.append(df)

    # Create MASTER file
    if all_data:
        master_df = pd.concat(all_data, ignore_index=True)
        master_path = os.path.join(root_folder, "MASTER_results.xlsx")
        master_df.to_excel(master_path, index=False)
        print(f"\nCreated MASTER file: {master_path}")

# ---------------------------
# RUN
# ---------------------------
if __name__ == "__main__":
    process_csv_files(ROOT_FOLDER)
