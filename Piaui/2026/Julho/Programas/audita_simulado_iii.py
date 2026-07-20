from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
PROFILES = ["2EM_LP", "2EM_MT", "3EM_LP", "3EM_MT"]


for profile in PROFILES:
    series = int(profile[0])
    discipline = profile.split("_")[1]
    tcm = ROOT / f"ResultadosTCM_{profile}" / "Simulado_III"
    tri = ROOT / f"ResultadosTRI_{profile}" / "Simulado_III"

    scores = pd.read_excel(tcm / "EscoresBrutos_Aluno.xlsx")
    measures = pd.read_excel(tcm / "MedidasTCM_Item.xlsx")
    theta = pd.read_excel(tri / f"ResumoTheta_{profile}_S3.xlsx", index_col=0).iloc[:, 0]
    items = pd.read_excel(tri / f"EstItens_{profile}_S3.xlsx")
    dist = pd.read_excel(tri / f"DistriAlunosClasse{discipline}{series}serie_S3.xlsx", index_col=0)

    tcm_items = set(measures["Item"].astype(int))
    tri_items = set(items["Item"].astype(int))
    excluded = sorted(tcm_items - tri_items)
    weak = measures[(measures["Discriminacao"] < 0.20) | (measures["CPBisserial"] < 0.20)].copy()
    hard = measures.nsmallest(5, "Dificuldade")

    print(f"\n### {profile}")
    print(
        "N",
        len(scores),
        "mean_score",
        round(float(scores.iloc[:, 1].mean()), 4),
        "pct",
        round(float(scores.iloc[:, 1].mean() / len(measures)), 4),
        "theta_mean",
        round(float(theta.loc["media"]), 4),
        "theta_dp",
        round(float(theta.loc["dp"]), 4),
        "theta_med",
        round(float(theta.loc["med."]), 4),
    )
    print("items TCM/TRI", len(tcm_items), len(tri_items), "excluded", excluded)
    print("weak TCM")
    print(weak[["Item", "Dificuldade", "Discriminacao", "CBisserial", "CPBisserial"]].to_string(index=False))
    print("hardest")
    print(hard[["Item", "Dificuldade", "Discriminacao", "CPBisserial"]].to_string(index=False))

    flags = []
    for label, subset in [
        ("b>400", items[items["bSAEB"] > 400]),
        ("a<.6", items[items["a"] < 0.60]),
        ("a>4", items[items["a"] > 4.00]),
        ("c>.25", items[items["c"] > 0.25]),
    ]:
        if not subset.empty:
            flags.append((label, [int(x) for x in subset["Item"].tolist()]))
    fixed = int((items["SAEB"] == "Sim").sum()) if "SAEB" in items else 0
    print("TRI flags", flags, "fixed", fixed)

    diagnostic_items = sorted(set(weak["Item"].astype(int).tolist()).union(excluded))
    if diagnostic_items:
        choice = pd.read_excel(tcm / "mDificNRDF.xlsx", index_col=0)
        pbis = pd.read_excel(tcm / "mcpBisNRDF.xlsx", index_col=0)
        discr = pd.read_excel(tcm / "mDiscNRDF.xlsx", index_col=0)
        for item_number in diagnostic_items:
            row_choice = choice.iloc[item_number - 1]
            row_pbis = pbis.iloc[item_number - 1]
            row_discr = discr.iloc[item_number - 1]
            print(f"ALT item {item_number} gabarito {row_choice['Gabarito']}")
            pieces = []
            for alternative in ["A", "B", "C", "D", "E"]:
                pieces.append(
                    f"{alternative}:{float(row_choice[alternative]):.1f}% "
                    f"pbis={float(row_pbis[alternative]):.3f} "
                    f"disc={float(row_discr[alternative]):.3f}"
                )
            print("; ".join(pieces))
    print("dist tail")
    print(dist.tail(3).to_string())
