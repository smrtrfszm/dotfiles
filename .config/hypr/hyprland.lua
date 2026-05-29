hl.config({
  input = {
    kb_layout = "hu",
    kb_options = "caps:ctrl_modifier",
    follow_mouse = 2,
    float_switch_override_focus = 0,
    focus_on_close = 0,
  },
  general = {
    layout = "master",
    gaps_in = 4,
    gaps_out = 8,
    allow_tearing = true,
  },
  animations = {
    enabled = true,
  },
  decoration = {
    rounding = 0,
    shadow = {
      enabled = false,
    },
    blur = {
      enabled = false,
    },
  },
  gestures = {
    workspace_swipe_min_speed_to_force = 5,
    workspace_swipe_invert = false,
  },
  misc = {
    disable_hyprland_logo = true,
    mouse_move_enables_dpms = true,
    key_press_enables_dpms = true,
    on_focus_under_fullscreen = 2,
    middle_click_paste = false,
    mouse_move_focuses_monitor = false,
  },
  render = {
    -- direct_scanout = true,
    non_shader_cm = 1,
  },
  master = {
    mfact = 0.5,
    new_on_active = "before",
  },
  cursor = {
      no_hardware_cursors = false,
      no_warps = true,
  },
  binds = {
    drag_threshold = 10
  },
})

hl.bind("SUPER + Return", hl.dsp.exec_cmd("kitty"))
hl.bind("SUPER + KP_Enter", hl.dsp.exec_cmd("kitty"))
hl.bind("SUPER + Backspace", hl.dsp.window.close())
hl.bind("SUPER + SHIFT + Backspace", hl.dsp.window.kill())
hl.bind("SUPER + B", hl.dsp.exec_cmd("flatpak run io.gitlab.librewolf-community"))

hl.bind("SUPER + O", hl.dsp.exec_cmd("rofi -show"))
hl.bind("SUPER + Escape", hl.dsp.exec_cmd("loginctl lock-session"))
hl.bind("SUPER + T", hl.dsp.window.float({ action = "toggle" }))
hl.bind("SUPER + F", hl.dsp.window.fullscreen({ mode = "maximized", action = "toggle" }))
hl.bind("SUPER + SHIFT + F", hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))
hl.bind("print", hl.dsp.exec_cmd("screenshot"))
hl.bind("SUPER + Space", hl.dsp.exec_cmd("exec makoctl dismiss --all"))
hl.bind("SUPER + SHIFT + Space", hl.dsp.exec_cmd("makoctl restore"))

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), { locked = true })
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), { locked = true })
hl.bind("XF86AudioMicMute",     hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"), { locked = true })

hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("light -A 5"), { locked = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("light -U 5"), { locked = true })

hl.bind("SUPER + SHIFT + M", hl.dsp.layout("swapwithmaster master"))
hl.bind("SUPER + Tab", hl.dsp.layout("cyclenext"))
hl.bind("SUPER + SHIFT + Tab", hl.dsp.layout("cycleprev"))

hl.bind("SUPER + 0", hl.dsp.focus({ workspace = 'name:0', on_current_monitor = true }))
hl.bind("SUPER + 1", hl.dsp.focus({ workspace = 1, on_current_monitor = true }))
hl.bind("SUPER + 2", hl.dsp.focus({ workspace = 2, on_current_monitor = true }))
hl.bind("SUPER + 3", hl.dsp.focus({ workspace = 3, on_current_monitor = true }))
hl.bind("SUPER + 4", hl.dsp.focus({ workspace = 4, on_current_monitor = true }))
hl.bind("SUPER + 5", hl.dsp.focus({ workspace = 5, on_current_monitor = true }))
hl.bind("SUPER + 6", hl.dsp.focus({ workspace = 6, on_current_monitor = true }))
hl.bind("SUPER + 7", hl.dsp.focus({ workspace = 7, on_current_monitor = true }))
hl.bind("SUPER + 8", hl.dsp.focus({ workspace = 8, on_current_monitor = true }))
hl.bind("SUPER + 9", hl.dsp.focus({ workspace = 9, on_current_monitor = true }))

hl.bind("SUPER + SHIFT + 0", hl.dsp.window.move({ workspace = 'name:0', follow = false }))
hl.bind("SUPER + SHIFT + 1", hl.dsp.window.move({ workspace = 1, follow = false }))
hl.bind("SUPER + SHIFT + 2", hl.dsp.window.move({ workspace = 2, follow = false }))
hl.bind("SUPER + SHIFT + 3", hl.dsp.window.move({ workspace = 3, follow = false }))
hl.bind("SUPER + SHIFT + 4", hl.dsp.window.move({ workspace = 4, follow = false }))
hl.bind("SUPER + SHIFT + 5", hl.dsp.window.move({ workspace = 5, follow = false }))
hl.bind("SUPER + SHIFT + 6", hl.dsp.window.move({ workspace = 6, follow = false }))
hl.bind("SUPER + SHIFT + 7", hl.dsp.window.move({ workspace = 7, follow = false }))
hl.bind("SUPER + SHIFT + 8", hl.dsp.window.move({ workspace = 8, follow = false }))
hl.bind("SUPER + SHIFT + 9", hl.dsp.window.move({ workspace = 9, follow = false }))

hl.bind("SUPER + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind("SUPER + mouse:273", hl.dsp.window.resize(), { mouse = true })

hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })

hl.animation({ leaf = "windowsIn", enabled = false })
hl.animation({ leaf = "windowsOut", enabled = false })
hl.animation({ leaf = "workspaces", enabled = true, speed = 2, bezier = "default" })
hl.animation({ leaf = "windowsMove", enabled = false })
hl.animation({ leaf = "fade", enabled = false })

hl.on("hyprland.start", function ()
  hl.exec_cmd("systemctl --user start hyprpolkitagent")
  hl.exec_cmd("gsettings set org.gnome.desktop.interface color-scheme prefer-dark")
  hl.exec_cmd("hyprctl setcursor Breeze 24")
  hl.exec_cmd("waybar")
  hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
  hl.exec_cmd("hypridle")
  hl.exec_cmd("hyprpaper")
end)

require("local.hyprland")
