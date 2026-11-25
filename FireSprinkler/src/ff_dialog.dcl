// ============================================================================
// FireSprinkler/src/ff_dialog.dcl - Modal Dialog Definition for FireSprinkler
// ============================================================================

ffsprinkler_dlg : dialog {
  label = "Fire Fighting Sprinkler Calculator (NFPA 13)";

  : boxed_column {
    label = "Room Info";
    : edit_box {
      key = "rtype";
      label = "Room Type:";
      edit_width = 30;
    }
    : row {
      : edit_box {
        key = "startnum";
        label = "Start Number:";
        edit_width = 10;
      }
      : edit_box {
        key = "height";
        label = "Font Size:";
        edit_width = 10;
      }
    }
    : edit_box {
      key = "extra";
      label = "Constant Text:";
      edit_width = 30;
    }
  }

  : boxed_column {
    label = "Area Calculation";
    : radio_row {
      label = "Unit Presets:";
      : radio_button { key = "p_mm";     label = "mm2->m2"; }
      : radio_button { key = "p_cm";     label = "cm2->m2"; }
      : radio_button { key = "p_m";      label = "m2->m2"; }
      : radio_button { key = "p_custom"; label = "Custom"; }
    }
    : row {
      : edit_box {
        key = "factor";
        label = "Scale Factor:";
        edit_width = 12;
      }
      : edit_box {
        key = "bias";
        label = "Area Bias (+/-):";
        edit_width = 10;
      }
    }
    : row {
      : radio_row {
        label = "Rounding:";
        : radio_button { key = "rnd_near"; label = "Nearest"; }
        : radio_button { key = "rnd_up";   label = "Up"; }
        : radio_button { key = "rnd_down"; label = "Down"; }
      }
      : edit_box {
        key = "prec";
        label = "Decimals:";
        edit_width = 5;
      }
    }
  }

  : boxed_column {
    label = "NFPA 13 Hazard & Sprinkler Coverage";
    : radio_row {
      label = "Hazard Class:";
      : radio_button { key = "h_light";  label = "Light (18.6)"; }
      : radio_button { key = "h_ord";    label = "Ordinary (12.1)"; }
      : radio_button { key = "h_extra";  label = "Extra (9.3)"; }
      : radio_button { key = "h_custom"; label = "Custom"; }
    }
    : edit_box {
      key = "cov_area";
      label = "Area / Sprinkler (m2):";
      edit_width = 12;
    }
    : text {
      key = "ff_hint";
      label = "Sprinklers = ceil(Room Area / Area per Sprinkler)  [rounded UP]";
    }
  }

  : boxed_column {
    label = "Output & Layers";
    : row {
      : popup_list {
        key = "mtext_layer";
        label = "MTEXT Layer:";
        edit_width = 22;
      }
      : toggle {
        key = "multiline";
        label = "Multi-line MTEXT";
      }
    }
    : row {
      : toggle {
        key = "keepbndry";
        label = "Keep Area Polyline";
      }
      : popup_list {
        key = "bndry_layer";
        label = "Polyline Layer:";
        edit_width = 22;
      }
    }
    : toggle {
      key = "fastbndry";
      label = "Fast Boundary (Skip Islands - click near wall)";
    }
  }

  : boxed_column {
    label = "Label Preview (sample raw area = 24.53 m2)";
    : text {
      key = "preview";
      width = 56;
    }
  }

  errtile;

  : row {
    fixed_width = true;
    alignment = centered;
    : button {
      key = "pick";
      label = "Pick Rooms";
      is_default = true;
      width = 14;
    }
    : button {
      key = "export";
      label = "Export Excel (CSV)";
      width = 18;
    }
    : button {
      key = "cancel";
      label = "Cancel";
      is_cancel = true;
      width = 12;
    }
  }
}
