import json
from pathlib import Path

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import seaborn as sns
from IPython.display import HTML, display

# Legend locations for matplotlib
# https://github.com/ebmdatalab/datalab-pandas/blob/master/ebmdatalab/charts.py
BEST = 0
UPPER_RIGHT = 1
UPPER_LEFT = 2
LOWER_LEFT = 3
LOWER_RIGHT = 4
RIGHT = 5
CENTER_LEFT = 6
CENTER_RIGHT = 7
LOWER_CENTER = 8
UPPER_CENTER = 9
CENTER = 10

BASE_DIR = Path(__file__).parents[3]
OUTPUT_DIR = BASE_DIR / "output/data"


def load_and_drop(measure, practice=False):
    """Loads the measure table for the measure with the given ID.

    Drops irrelevant practices and casts the `date` column from a `str`
    to a `datetime64`.

    Args:
        measure: The measure ID.
        practice: Whether to load the "practice only" measure.

    Returns:
        The table for the given measure ID and practice.
    """
    if practice:
        f_in = OUTPUT_DIR / f"measure_{measure}_practice_only.csv"
    else:
        f_in = OUTPUT_DIR / f"measure_{measure}.csv"

    df = pd.read_csv(f_in, parse_dates=["date"])
    df = drop_irrelevant_practices(df)
    return df


def calculate_rate(df, value_col, population_col):
    """Calculates the number of events per 1,000 of the population.

    This function operates on the given measure table in-place, adding
    a `num_per_thousand` column.

    Args:
        df: A measure table.
        value_col: The name of the numerator column in the measure table.
        population_col: The name of the denominator column in the measure table.
    """
    num_per_thousand = df[value_col] / (df[population_col] / 1000)
    df["num_per_thousand"] = num_per_thousand


def drop_irrelevant_practices(df):
    """Drops irrelevant practices from the given measure table.

    An irrelevant practice has zero events during the study period.

    Args:
        df: A measure table.

    Returns:
        A copy of the given measure table with irrelevant practices dropped.
    """
    is_relevant = df.groupby("practice").value.any()
    return df[df.practice.isin(is_relevant[is_relevant == True].index)]


def create_child_table(
    df, code_df, code_column, term_column, measure, nrows=5
):
    """
    Args:
        df: A measure table.
        code_df: A codelist table.
        code_column: The name of the code column in the codelist table.
        term_column: The name of the term column in the codelist table.
        measure: The measure ID.
        nrows: The number of rows to display.

    Returns:
        A table of the top `nrows` codes.
    """
    event_counts = (
        df.groupby(f"{measure}_event_code")[f"{measure}"]
        .sum()  # We can't use .count() because the measure column contains zeros.
        .rename_axis(code_column)
        .rename("Events")
        .reset_index()
        .sort_values("Events", ascending=False)
    )

    event_counts["Events (thousands)"] = event_counts["Events"] / 1000

    # Gets the human-friendly description of the code for the given row
    # e.g. "Systolic blood pressure".
    code_df = code_df.set_index(code_column).rename(
        columns={term_column: "Description"}
    )
    event_counts = (
        event_counts.set_index(code_column).join(code_df).reset_index()
    )

    # Cast the code to an integer.
    event_counts[code_column] = event_counts[code_column].astype(int)

    # return top n rows
    return event_counts.iloc[:nrows, :]


def get_number_practices(df):
    """Gets the number of practices in the given measure table.

    Args:
        df: A measure table.
    """
    return len(df.practice.unique())


def get_percentage_practices(measure_table):
    """Gets the percentage of practices in the given measure table.

    Args:
        measure_table: A measure table.
    """
    with open(OUTPUT_DIR / "practice_count.json") as f:
        num_practices = json.load(f)["num_practices"]

    num_practices_in_study = get_number_practices(measure_table)

    return (num_practices_in_study / num_practices) * 100


def get_number_events_mil(measure_table, measure_id):
    """Gets the number of events per million, rounded to 2DP.

    Args:
        measure_table: A measure table.
        measure_id: The measure ID.
    """
    return np.round(measure_table[measure_id].sum() / 1_000_000, 2)


def get_number_patients(measure_id):
    """Gets the number of patients.

    Args:
        measure_id: The measure ID.
    """
    with open(OUTPUT_DIR / "patient_count.json") as f:
        d = json.load(f)
    return d["num_patients"][measure_id]


# https://github.com/ebmdatalab/datalab-pandas/blob/master/ebmdatalab/charts.py
def deciles_chart_ebm(
    df,
    period_column=None,
    column=None,
    title="",
    ylabel="",
    show_outer_percentiles=True,
    show_legend=True,
    ax=None,
):
    """period_column must be dates / datetimes"""
    sns.set_style("whitegrid", {"grid.color": ".9"})
    if not ax:
        fig, ax = plt.subplots(1, 1)
    df = add_percentiles(
        df,
        period_column=period_column,
        column=column,
        show_outer_percentiles=show_outer_percentiles,
    )
    linestyles = {
        "decile": {
            "line": "b--",
            "linewidth": 1,
            "label": "decile",
        },
        "median": {
            "line": "b-",
            "linewidth": 1.5,
            "label": "median",
        },
        "percentile": {
            "line": "b:",
            "linewidth": 0.8,
            "label": "1st-9th, 91st-99th percentile",
        },
    }
    label_seen = []
    for percentile in range(1, 100):  # plot each decile line
        data = df[df["percentile"] == percentile]
        add_label = False

        if percentile == 50:
            style = linestyles["median"]
            add_label = True
        elif show_outer_percentiles and (percentile < 10 or percentile > 90):
            style = linestyles["percentile"]
            if "percentile" not in label_seen:
                label_seen.append("percentile")
                add_label = True
        else:
            style = linestyles["decile"]
            if "decile" not in label_seen:
                label_seen.append("decile")
                add_label = True
        if add_label:
            label = style["label"]
        else:
            label = "_nolegend_"

        ax.plot(
            data[period_column],
            data[column],
            style["line"],
            linewidth=style["linewidth"],
            label=label,
        )
    ax.set_ylabel(ylabel, size=15, alpha=0.6)
    if title:
        ax.set_title(title, size=18)
    # set ymax across all subplots as largest value across dataset
    ax.set_ylim([0, df[column].max() * 1.05])
    ax.tick_params(labelsize=12)
    ax.set_xlim(
        [df[period_column].min(), df[period_column].max()]
    )  # set x axis range as full date range

    plt.setp(ax.xaxis.get_majorticklabels(), rotation=90)
    ax.xaxis.set_major_formatter(matplotlib.dates.DateFormatter("%B %Y"))
    if show_legend:
        ax.legend(
            bbox_to_anchor=(1.1, 0.8),  # arbitrary location in axes
            #  specified as (x0, y0, w, h)
            loc=CENTER_LEFT,  # which part of the bounding box should
            #  be placed at bbox_to_anchor
            ncol=1,  # number of columns in the legend
            fontsize=12,
            borderaxespad=0.0,
        )  # padding between the axes and legend
        #  specified in font-size units
    # rotates and right aligns the x labels, and moves the bottom of the
    # axes up to make room for them
    plt.gcf().autofmt_xdate()
    return plt


def add_percentiles(
    df, period_column=None, column=None, show_outer_percentiles=True
):
    """For each period in `period_column`, compute percentiles across that
    range.
    Adds `percentile` column.
    """
    deciles = np.arange(0.1, 1, 0.1)
    bottom_percentiles = np.arange(0.01, 0.1, 0.01)
    top_percentiles = np.arange(0.91, 1, 0.01)
    if show_outer_percentiles:
        quantiles = np.concatenate(
            (deciles, bottom_percentiles, top_percentiles)
        )
    else:
        quantiles = deciles
    df = df.groupby(period_column)[column].quantile(quantiles).reset_index()
    df = df.rename(index=str, columns={"level_1": "percentile"})
    # create integer range of percentiles
    df["percentile"] = df["percentile"].apply(lambda x: int(x * 100))
    return df


def deciles_chart(
    df, period_column=None, column=None, title="", ylabel="", interactive=True
):
    """period_column must be dates / datetimes"""

    df = add_percentiles(
        df,
        period_column=period_column,
        column=column,
        show_outer_percentiles=False,
    )

    if interactive:

        fig = go.Figure()

        linestyles = {
            "decile": {"color": "blue", "dash": "dash"},
            "median": {"color": "blue", "dash": "solid"},
            "percentile": {"color": "blue", "dash": "dash"},
        }

        for percentile in np.unique(df["percentile"]):
            df_subset = df[df["percentile"] == percentile]
            if percentile == 50:
                fig.add_trace(
                    go.Scatter(
                        x=df_subset[period_column],
                        y=df_subset[column],
                        line={"color": "blue", "dash": "solid", "width": 1.2},
                        name="median",
                    )
                )
            else:
                fig.add_trace(
                    go.Scatter(
                        x=df_subset[period_column],
                        y=df_subset[column],
                        line={"color": "blue", "dash": "dash", "width": 1},
                        name=f"decile {int(percentile/10)}",
                    )
                )

        # Set title
        fig.update_layout(
            title_text=title,
            hovermode="x",
            title_x=0.5,
        )

        fig.update_yaxes(title=ylabel)
        fig.update_xaxes(title="Date")

        # Add range slider
        fig.update_layout(
            xaxis=go.layout.XAxis(
                rangeselector=dict(
                    buttons=list(
                        [
                            dict(
                                count=1,
                                label="1m",
                                step="month",
                                stepmode="backward",
                            ),
                            dict(
                                count=6,
                                label="6m",
                                step="month",
                                stepmode="backward",
                            ),
                            dict(
                                count=1,
                                label="1y",
                                step="year",
                                stepmode="backward",
                            ),
                            dict(step="all"),
                        ]
                    )
                ),
                rangeslider=dict(visible=True),
                type="date",
            )
        )

        fig.show()

    else:
        deciles_chart_ebm(
            df,
            period_column="date",
            column="num_per_thousand",
            ylabel="rate per 1000",
            show_outer_percentiles=False,
        )


def generate_sentinel_measure(
    data_dict,
    data_dict_practice,
    codelist_dict,
    measure,
    code_column,
    term_column,
    dates_list,
    interactive=True,
):
    """Generates tables and charts for the measure with the given ID.

    Args:
        data_dict: A mapping of measure IDs to measure tables.
        data_dict_practice: A mapping of measure IDs to "practice only" measure tables.
        codelist_dict: A mapping of measure IDs to codelist tables.
        measure: A measure ID.
        code_column: The name of the code column in the codelist table.
        term_column: The name of the term column in the codelist table.
        dates_list: Not used.
        interactive: Flag indicating whether or not the chart should be interactive.
    """
    df = data_dict[measure]
    childs_df = create_child_table(
        df, codelist_dict[measure], code_column, term_column, measure
    )

    practices_included = get_number_practices(df)
    practices_included_percent = get_percentage_practices(df)
    num_events_mil = get_number_events_mil(df, measure)
    num_patients = get_number_patients(measure)

    print(
        f"Practices included: {practices_included} ({practices_included_percent}%)"
    )
    print(
        f"Total patients: {num_patients:.2f}M ({num_events_mil:.2f}M events)"
    )

    df = data_dict_practice[measure]
    calculate_rate(df, measure, "population")

    display(HTML(childs_df.to_html()))

    deciles_chart(
        df,
        period_column="date",
        column="num_per_thousand",
        ylabel="rate per 1000",
        interactive=interactive,
    )
