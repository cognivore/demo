{- |
Module      : PanharmoniconDeploymentPresentation
Description : Deep dive into the panharmonicon deployment system
Copyright   : (c) Geosurge, 2026
License     : MIT

A comprehensive presentation demonstrating the panharmonicon deployment
infrastructure from start to finish. Shows each raw command that gets
executed - no wrapper scripts, just the actual operations.

Run with: demo ./examples/PanharmoniconDeploymentPresentation.hs
-}
module PanharmoniconDeploymentPresentation (presentation) where

import Demo.Core.DSL

presentation :: Presentation
presentation = mkPresentation "Panharmonicon Deployment: One Command to Rule Them All" $ do
    ---------------------------------------------------------------------------
    -- Opening: Check Current State
    ---------------------------------------------------------------------------

    slide "Starting Fresh: Current State" $ do
        note $
            unlines
                [ "Welcome! We're going to walk through the entire panharmonicon"
                , "deployment system from scratch."
                , ""
                , "Let's first check what's currently deployed."
                ]
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"

    ---------------------------------------------------------------------------
    -- Section 1: Developer Isolation
    ---------------------------------------------------------------------------

    slide "Developer Isolation: The Problem" $ do
        note $
            unlines
                [ "How do we let multiple developers deploy independently"
                , "without stepping on each other?"
                , ""
                , "Solution: derive a unique project ID from each developer's"
                , "git email using SHA256, truncated to 8 characters."
                ]
        system "echo 'Step 1: Get git email'"
        system "git config user.email"
        system "echo ''"
        system "echo 'Step 2: Hash it'"
        system "printf '%s' \"$(git config user.email)\" | shasum -a 256"
        system "echo ''"
        system "echo 'Step 3: Take first 8 chars'"
        system "printf '%s' \"$(git config user.email)\" | shasum -a 256 | cut -c1-8"

    slide "Developer Isolation: Project ID" $ do
        note $
            unlines
                [ "The project ID is: panharmonicon-<dev_id>"
                , ""
                , "alice@company.com -> panharmonicon-d0eeac2a"
                , "bob@company.com   -> panharmonicon-7f3e1b2c"
                , ""
                , "Complete isolation. Each dev gets their own GCP project."
                ]
        elaborate "apps/panharmonicon/deploy/deploy.sh" (35, 45) "Developer ID generation"
        system "DEV_ID=$(printf '%s' \"$(git config user.email)\" | shasum -a 256 | cut -c1-8)"
        system "echo \"Project ID: panharmonicon-$(printf '%s' \"$(git config user.email)\" | shasum -a 256 | cut -c1-8)\""
        system "echo \"State bucket: panharmonicon-$(printf '%s' \"$(git config user.email)\" | shasum -a 256 | cut -c1-8)-tfstate\""

    ---------------------------------------------------------------------------
    -- Section 2: System Detection
    ---------------------------------------------------------------------------

    slide "The Cross-Compilation Problem" $ do
        note $
            unlines
                [ "We're building NixOS (x86_64-linux) from a MacBook (aarch64-darwin)."
                , ""
                , "Nix can't cross-compile an entire Linux system from macOS."
                , "We need an actual Linux machine for the heavy lifting."
                , ""
                , "Let's detect what system we're on."
                ]
        system "echo 'Detecting system...'"
        system "OS=$(uname -s)"
        system "ARCH=$(uname -m)"
        system "echo \"OS: $(uname -s)\""
        system "echo \"Arch: $(uname -m)\""
        system "echo ''"
        system "case \"$(uname -s)-$(uname -m)\" in Darwin-arm64) echo 'System: aarch64-darwin';; Darwin-x86_64) echo 'System: x86_64-darwin';; Linux-x86_64) echo 'System: x86_64-linux';; *) echo 'System: unknown';; esac"

    slide "Do We Need a Builder?" $ do
        note $
            unlines
                [ "If we're NOT on x86_64-linux, we need a remote builder."
                , ""
                , "On macOS (either arch): YES, need builder"
                , "On aarch64-linux: YES, need builder"
                , "On x86_64-linux: NO, can build locally"
                ]
        elaborate "apps/panharmonicon/deploy/deploy.sh" (61, 65) "Builder detection"
        system "SYSTEM=$(case \"$(uname -s)-$(uname -m)\" in Darwin-arm64) echo 'aarch64-darwin';; Darwin-x86_64) echo 'x86_64-darwin';; Linux-x86_64) echo 'x86_64-linux';; *) echo 'unknown';; esac)"
        system "echo \"Current system: $(case \"$(uname -s)-$(uname -m)\" in Darwin-arm64) echo 'aarch64-darwin';; Darwin-x86_64) echo 'x86_64-darwin';; Linux-x86_64) echo 'x86_64-linux';; *) echo 'unknown';; esac)\""
        system "if [ \"$(case \"$(uname -s)-$(uname -m)\" in Darwin-arm64) echo 'aarch64-darwin';; Darwin-x86_64) echo 'x86_64-darwin';; Linux-x86_64) echo 'x86_64-linux';; *) echo 'unknown';; esac)\" != 'x86_64-linux' ]; then echo 'NEEDS BUILDER: YES'; else echo 'NEEDS BUILDER: NO'; fi"

    ---------------------------------------------------------------------------
    -- Section 3: Builder VM Provisioning
    ---------------------------------------------------------------------------

    slide "Builder: Check If VM Exists" $ do
        note $
            unlines
                [ "First, check if a builder VM already exists."
                , ""
                , "We look for terraform.tfstate in the builder directory"
                , "and try to get the IP from Terraform output."
                ]
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy/builder"
        system "echo 'Checking for existing builder state...'"
        system "ls -la ~/Github/grim-monolith/apps/panharmonicon/deploy/builder/terraform.tfstate 2>/dev/null && echo 'State file exists' || echo 'No state file - builder not provisioned'"
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy/builder && tofu output -raw builder_ip 2>/dev/null && echo '' || echo 'No builder IP - not provisioned'"

    slide "Builder: Terraform Init" $ do
        note $
            unlines
                [ "If no builder exists, we provision one."
                , ""
                , "Step 1: Initialize Terraform in the builder directory."
                , "This downloads the Google provider."
                ]
        elaborate "apps/panharmonicon/deploy/builder/main.tf" (8, 17) "Terraform config"
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy/builder && tofu init 2>&1 | tail -10"

    slide "Builder: Terraform Variables" $ do
        note $
            unlines
                [ "The builder needs:"
                , "  - project_id: your developer project"
                , "  - ssh_public_key: your SSH key for access"
                , ""
                , "Let's see what we'd pass."
                ]
        system "echo 'Variables for builder:'"
        system "echo \"  project_id = panharmonicon-$(printf '%s' \"$(git config user.email)\" | shasum -a 256 | cut -c1-8)\""
        system "echo \"  ssh_public_key = $(cat ~/.ssh/id_ed25519.pub 2>/dev/null | head -c 50)...\""

    slide "Builder: Terraform Apply (What It Creates)" $ do
        note $
            unlines
                [ "tofu apply creates:"
                , ""
                , "  1. Static IP (google_compute_address)"
                , "  2. Firewall rule for SSH (google_compute_firewall)"
                , "  3. Debian VM with Nix (google_compute_instance)"
                , ""
                , "The VM specs: n2-standard-8 (8 vCPU, 32GB), 100GB SSD"
                ]
        elaborate "apps/panharmonicon/deploy/builder/main.tf" (79, 95) "VM resource"
        system "grep -E '(machine_type|boot_disk_gb)' ~/Github/grim-monolith/apps/panharmonicon/deploy/builder/main.tf | head -5"

    slide "Builder: Cloud-Init Script" $ do
        note $
            unlines
                [ "When the VM boots, a startup script runs:"
                , ""
                , "  1. apt-get install curl xz-utils sudo git"
                , "  2. Create builder user with sudo"
                , "  3. Setup SSH authorized_keys"
                , "  4. Install Nix: curl -L https://nixos.org/nix/install | sh"
                , "  5. Configure /etc/nix/nix.conf"
                , "  6. Pre-warm cache: nix build nixpkgs#hello"
                ]
        elaborate "apps/panharmonicon/deploy/builder/main.tf" (148, 156) "Nix installation"
        system "grep -A 5 'Install Nix' ~/Github/grim-monolith/apps/panharmonicon/deploy/builder/main.tf"

    ---------------------------------------------------------------------------
    -- Section 4: Local Configuration
    ---------------------------------------------------------------------------

    slide "Local Config: SSH Known Hosts" $ do
        note $
            unlines
                [ "After the VM exists, we configure our local machine."
                , ""
                , "Step 1: Add the builder's SSH host key to known_hosts"
                , "This prevents 'host key verification failed' errors."
                , ""
                , "We add it to both:"
                , "  ~/.ssh/known_hosts (for our user)"
                , "  /var/root/.ssh/known_hosts (for nix-daemon)"
                ]
        system "echo 'Current known_hosts entries for builder:'"
        system "grep -c 'PANHARMONICON-BUILDER' ~/.ssh/known_hosts 2>/dev/null || echo '0 entries'"

    slide "Local Config: SSH Alias" $ do
        note $
            unlines
                [ "Step 2: Create an SSH alias 'nix-builder'"
                , ""
                , "This goes into ~/.ssh/config:"
                , ""
                , "  Host nix-builder"
                , "    HostName <builder-ip>"
                , "    User root"
                , "    IdentityFile ~/.ssh/id_ed25519"
                , ""
                , "Now 'ssh nix-builder' just works."
                ]
        elaborate "apps/panharmonicon/deploy/builder/provision-builder.sh" (143, 151) "SSH config block"
        system "echo 'Current SSH config for nix-builder:'"
        system "grep -A 6 'Host nix-builder' ~/.ssh/config 2>/dev/null || echo 'Not configured'"

    slide "Local Config: Root SSH Config" $ do
        note $
            unlines
                [ "Step 3: Same SSH config for root"
                , ""
                , "The nix-daemon runs as root on macOS."
                , "It needs to SSH to the builder too."
                , ""
                , "We copy our SSH key to /var/root/.ssh/id_ed25519"
                , "and add the SSH config to /var/root/.ssh/config"
                ]
        system "echo 'Root SSH key:'"
        system "sudo ls -la /var/root/.ssh/id_ed25519 2>/dev/null || echo 'Not present'"
        system "echo ''"
        system "echo 'Root SSH config for nix-builder:'"
        system "sudo grep -A 6 'Host nix-builder' /var/root/.ssh/config 2>/dev/null || echo 'Not configured'"

    slide "Local Config: Nix Builder Config" $ do
        note $
            unlines
                [ "Step 4: Tell Nix about the builder"
                , ""
                , "Add to /etc/nix/nix.custom.conf:"
                , ""
                , "  trusted-users = root <username>"
                , "  builders = ssh://nix-builder x86_64-linux - 8 1 big-parallel"
                , "  builders-use-substitutes = true"
                , ""
                , "When Nix needs x86_64-linux, it SSHs to nix-builder."
                ]
        elaborate "apps/panharmonicon/deploy/builder/provision-builder.sh" (117, 128) "Nix config"
        system "echo 'Current Nix builder config:'"
        system "cat /etc/nix/nix.custom.conf 2>/dev/null || echo 'Not configured'"

    slide "Local Config: Restart Nix Daemon" $ do
        note $
            unlines
                [ "Step 5: Restart nix-daemon to pick up changes"
                , ""
                , "On macOS with Determinate Nix:"
                , "  sudo launchctl kickstart -k system/systems.determinate.nix-daemon"
                , ""
                , "On macOS with standard Nix:"
                , "  sudo launchctl kickstart -k system/org.nixos.nix-daemon"
                ]
        system "echo 'Nix daemon service:'"
        system "sudo launchctl list | grep nix-daemon | head -1 || echo 'Not found'"

    ---------------------------------------------------------------------------
    -- Section 5: Idempotency & Reversibility
    ---------------------------------------------------------------------------

    slide "Idempotent Markers" $ do
        note $
            unlines
                [ "Every config change uses markers:"
                , ""
                , "  # PANHARMONICON-BUILDER:1.2.3.4"
                , "  ... config lines ..."
                , "  # END PANHARMONICON-BUILDER:1.2.3.4"
                , ""
                , "This enables:"
                , "  - Idempotent adds (check if marker exists first)"
                , "  - Clean removal (awk deletes between markers)"
                ]
        elaborate "apps/panharmonicon/deploy/builder/provision-builder.sh" (276, 291) "AWK removal"
        system "echo 'Marker pattern in known_hosts:'"
        system "grep 'PANHARMONICON-BUILDER' ~/.ssh/known_hosts 2>/dev/null | head -2 || echo 'No markers'"

    ---------------------------------------------------------------------------
    -- Section 6: Terragrunt & Infrastructure
    ---------------------------------------------------------------------------

    slide "Terragrunt: Dynamic Configuration" $ do
        note $
            unlines
                [ "Now for the main infrastructure."
                , ""
                , "Terragrunt wraps Terraform and computes values dynamically:"
                , "  - dev_id from git email hash"
                , "  - project_id = panharmonicon-<dev_id>"
                , "  - bucket_name = <project_id>-tfstate"
                , ""
                , "It also detects if the bucket exists to choose backend."
                ]
        elaborate "apps/panharmonicon/deploy/terraform/terragrunt.hcl" (12, 30) "Dynamic locals"
        system "head -30 ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/terragrunt.hcl"

    slide "Terragrunt: Billing Account" $ do
        note $
            unlines
                [ "The billing account is hardcoded - same for everyone."
                , "Developers don't need to know or manage billing."
                , ""
                , "  org_id = \"929673943880\""
                , "  billing_account = \"01E906-A607DF-19CDD0\""
                ]
        elaborate "apps/panharmonicon/deploy/terraform/terragrunt.hcl" (50, 59) "Inputs"
        system "grep -E '(org_id|billing_account)' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/terragrunt.hcl"

    slide "Terragrunt: Init" $ do
        note $
            unlines
                [ "First run: terragrunt init"
                , ""
                , "If state bucket doesn't exist, uses local backend."
                , "If state bucket exists, uses GCS backend."
                , ""
                , "The -migrate-state flag auto-migrates on backend change."
                ]
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform && terragrunt init 2>&1 | tail -15"

    ---------------------------------------------------------------------------
    -- Section 6b: Cross-Laptop Deployment
    ---------------------------------------------------------------------------

    slide "Cross-Laptop: The State Problem" $ do
        note $
            unlines
                [ "What happens when you deploy from laptop A,"
                , "then want to update from laptop B?"
                , ""
                , "Terraform state must be shared. Otherwise laptop B"
                , "thinks nothing exists and tries to recreate everything!"
                , ""
                , "Solution: GCS bucket for state storage."
                ]
        system "echo 'State location options:'"
        system "echo '  Local:  .terraform.tfstate (laptop-specific, BAD)'"
        system "echo '  GCS:    gs://panharmonicon-<hash>-tfstate/ (shared, GOOD)'"

    slide "Cross-Laptop: Automatic Migration" $ do
        note $
            unlines
                [ "The deploy.sh handles this automatically:"
                , ""
                , "  1. First apply: local state, creates bucket"
                , "  2. After apply: detect bucket was just created"
                , "  3. Re-run terragrunt init to migrate state to GCS"
                , "  4. Now any laptop with same git email can deploy!"
                , ""
                , "All in a single ./deploy.sh apply."
                ]
        elaborate "apps/panharmonicon/deploy/deploy.sh" (146, 172) "State migration"
        system "echo 'Migration logic:'"
        system "echo '  bucket_existed_before=false  # check BEFORE apply'"
        system "echo '  terragrunt apply'"
        system "echo '  if bucket just created:'"
        system "echo '    terragrunt init -force-copy  # migrate to GCS'"

    slide "Cross-Laptop: Same Git Email = Same Project" $ do
        note $
            unlines
                [ "Key insight: project ID derived from git email."
                , ""
                , "  Laptop A: git config user.email = alice@foo.com"
                , "  Laptop B: git config user.email = alice@foo.com"
                , ""
                , "  Both get: panharmonicon-d0eeac2a"
                , "  Both use: gs://panharmonicon-d0eeac2a-tfstate/"
                , ""
                , "Same state bucket = same infrastructure view."
                ]
        system "echo 'Your project ID:'"
        system "echo \"  panharmonicon-$(printf '%s' \"$(git config user.email)\" | shasum -a 256 | cut -c1-8)\""
        system "echo ''"
        system "echo 'Your state bucket:'"
        system "echo \"  gs://panharmonicon-$(printf '%s' \"$(git config user.email)\" | shasum -a 256 | cut -c1-8)-tfstate/\""

    slide "Cross-Laptop: Verify State Location" $ do
        note $
            unlines
                [ "The status command shows where state lives:"
                , ""
                , "  State bucket: OK (GCS backend)  <- Good!"
                , "  State bucket: OK (but using local state!)  <- Run init"
                , ""
                , "If you see 'local state', run:"
                , "  cd terraform && terragrunt init -force-copy"
                ]
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"
        system "echo ''"
        system "echo 'Current backend.tf:'"
        system "head -7 ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/backend.tf"

    slide "Terraform: Project Creation" $ do
        note $
            unlines
                [ "google_project.dev creates the GCP project itself."
                , ""
                , "It passes:"
                , "  - org_id: which GCP organization"
                , "  - billing_account: who pays"
                , ""
                , "Project is linked to billing at creation time."
                ]
        elaborate "apps/panharmonicon/deploy/terraform/main.tf" (21, 30) "Project resource"
        system "grep -A 10 'resource \"google_project\" \"dev\"' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/main.tf"

    slide "Terraform: APIs & Buckets" $ do
        note $
            unlines
                [ "After project creation:"
                , ""
                , "  1. Enable APIs: compute, secretmanager, storage"
                , "  2. Create tfstate bucket for Terraform state"
                , "  3. Create content bucket for generated articles"
                ]
        system "grep 'google_project_service' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/main.tf | head -5"
        system "echo ''"
        system "grep -A 5 'google_storage_bucket.*tfstate' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/main.tf | head -6"

    slide "Terraform: VM & Network" $ do
        note $
            unlines
                [ "The main VM resources:"
                , ""
                , "  google_compute_address - static IP"
                , "  google_compute_instance - the NixOS VM"
                , "  google_compute_firewall - allow SSH"
                ]
        elaborate "apps/panharmonicon/deploy/terraform/main.tf" (96, 142) "Compute instance"
        system "grep -B 2 'google_compute_instance' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/main.tf | head -5"

    slide "Terraform: nixos-anywhere Module" $ do
        note $
            unlines
                [ "The magic: nixos-anywhere Terraform module"
                , ""
                , "It takes a Debian VM and converts it to NixOS:"
                , "  1. SSHs to the VM"
                , "  2. Partitions disk via disko"
                , "  3. Installs NixOS from our flake"
                , "  4. Reboots into NixOS"
                ]
        elaborate "apps/panharmonicon/deploy/terraform/main.tf" (148, 163) "nixos-anywhere module"
        system "grep -A 15 'module \"nixos_deploy\"' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/main.tf"

    slide "Terragrunt: Apply" $ do
        note $
            unlines
                [ "terragrunt apply does everything:"
                , ""
                , "  1. Creates GCP project (if needed)"
                , "  2. Creates buckets"
                , "  3. Creates VM with Debian"
                , "  4. nixos-anywhere converts to NixOS"
                , "  5. Deploys our configuration"
                , ""
                , "This takes 5-10 minutes on first run."
                ]
        system "echo 'Would run: cd terraform && terragrunt apply -auto-approve'"
        system "echo ''"
        system "echo 'Current state:'"
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"

    ---------------------------------------------------------------------------
    -- Section 7: NixOS Configuration
    ---------------------------------------------------------------------------

    slide "NixOS: Declarative System" $ do
        note $
            unlines
                [ "The NixOS configuration is fully declarative."
                , ""
                , "Arguments received:"
                , "  - panharmonicon: the Haskell package"
                , "  - panharmoniconData: pipeline dossiers"
                , ""
                , "Imports: hardware.nix, disko.nix"
                ]
        elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (1, 17) "Config header"
        system "head -20 ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix"

    slide "NixOS: Agenix Secrets" $ do
        note $
            unlines
                [ "Secrets are encrypted with age."
                , ""
                , "The VM's SSH host key is a recipient."
                , "Only that specific VM can decrypt."
                , ""
                , "  owner = \"panharmonicon\""
                , "  group = \"panharmonicon\""
                , "  mode = \"0400\""
                ]
        elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (47, 53) "Agenix secret"
        system "grep -A 6 'age.secrets.openai-key' ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix"

    slide "NixOS: Data Symlinks" $ do
        note $
            unlines
                [ "Pipeline data comes from the Nix store (read-only)."
                , ""
                , "systemd.tmpfiles.rules creates symlinks:"
                , "  /var/lib/panharmonicon/priv/Pipeline -> nix store"
                , "  /var/lib/panharmonicon/priv/outputs -> writable"
                ]
        elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (55, 65) "Tmpfiles"
        system "grep -A 8 'tmpfiles.rules' ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix"

    slide "NixOS: Systemd Service" $ do
        note $
            unlines
                [ "The panharmonicon-daisychain service:"
                , ""
                , "  1. export OPENAI_API_KEY=$(cat /run/agenix/openai-key)"
                , "  2. Get PROJECT_ID from GCP metadata"
                , "  3. export PANHARMONICON_GCS_BUCKET=${PROJECT_ID}-content"
                , "  4. Run panharmonicon-llm-daisychain"
                , "  5. gsutil rsync to GCS"
                ]
        elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (82, 96) "Service script"
        system "grep -A 15 'script = ' ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix | head -15"

    ---------------------------------------------------------------------------
    -- Section 8: Package & Dependencies
    ---------------------------------------------------------------------------

    slide "Package: Flake Inputs" $ do
        note $
            unlines
                [ "Git dependencies are flake inputs:"
                , ""
                , "  haskllm.url = \"github:geosurge-ai/haskllm\""
                , "  pandoc-command-simple.url = \"...\""
                , ""
                , "Locked in flake.lock. Update with:"
                , "  nix flake update haskllm pandoc-command-simple"
                ]
        elaborate "flake.nix" (24, 27) "Flake inputs"
        system "grep -A 3 'Panharmonicon dependencies' ~/Github/grim-monolith/flake.nix"

    slide "Package: dontCheck" $ do
        note $
            unlines
                [ "Tests need network/API keys."
                , "Nix sandbox has neither."
                , ""
                , "Solution: pkgs.haskell.lib.dontCheck"
                , ""
                , "Tests run in CI with secrets, not in nix build."
                ]
        elaborate "apps/panharmonicon/package.nix" (24, 33) "dontCheck"
        system "grep -B 1 'dontCheck' ~/Github/grim-monolith/apps/panharmonicon/package.nix"

    slide "Package: Data Derivation" $ do
        note $
            unlines
                [ "Pipeline dossiers bundled into a derivation:"
                , ""
                , "  panharmoniconData = pkgs.runCommand ... ''"
                , "    cp -r priv/Pipeline $out/Pipeline"
                , "    cp -r priv/cache $out/cache"
                , "  ''"
                , ""
                , "Immutable paths in the Nix store."
                ]
        elaborate "apps/panharmonicon/package.nix" (36, 48) "Data derivation"
        system "grep -A 6 'panharmoniconData' ~/Github/grim-monolith/apps/panharmonicon/package.nix"

    ---------------------------------------------------------------------------
    -- Section 9: Verification
    ---------------------------------------------------------------------------

    slide "Verify: Current Status" $ do
        note $
            unlines
                [ "Let's see what's deployed right now."
                ]
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"

    slide "Verify: GCS Contents" $ do
        note $
            unlines
                [ "Check what's in the content bucket."
                ]
        system "gsutil ls gs://panharmonicon-*-content/ 2>/dev/null | head -20 || echo 'No content bucket or empty'"

    ---------------------------------------------------------------------------
    -- Closing: Destroy
    ---------------------------------------------------------------------------

    slide "Closing: We Will Now Destroy Everything" $ do
        note $
            unlines
                [ "That's the complete system!"
                , ""
                , "Recap:"
                , "  - Per-developer GCP projects (git email hash)"
                , "  - Automatic Linux builder provisioning"
                , "  - Idempotent local config with markers"
                , "  - NixOS via nixos-anywhere"
                , "  - Agenix secrets"
                , "  - Data in Nix store, symlinked at runtime"
                , "  - Auto GCS sync"
                , "  - Cross-laptop: state auto-migrates to GCS"
                , ""
                , "Now we'll destroy and verify in the GCP console."
                ]
        system "echo ''"
        system "echo '  ╔══════════════════════════════════════════════════════════════╗'"
        system "echo '  ║  Ready to destroy. Check GCP console after to verify.        ║'"
        system "echo '  ╚══════════════════════════════════════════════════════════════╝'"
        system "echo ''"

    slide "Destroy: Terraform Destroy" $ do
        note $
            unlines
                [ "Step 1: Destroy the panharmonicon VM"
                , ""
                , "  cd terraform && terragrunt destroy -auto-approve"
                , ""
                , "This removes:"
                , "  - The VM"
                , "  - The static IP"
                , "  - Firewall rules"
                , ""
                , "Preserves: GCP project, state bucket, content bucket"
                ]
        system "echo 'Would run: cd terraform && terragrunt destroy -auto-approve'"
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform && terragrunt output 2>/dev/null || echo 'No outputs - not deployed'"

    slide "Destroy: Builder Cleanup" $ do
        note $
            unlines
                [ "Step 2: Destroy the builder VM"
                , ""
                , "  cd builder && tofu destroy -auto-approve"
                , ""
                , "Step 3: Remove local config"
                , ""
                , "  - Remove from ~/.ssh/known_hosts"
                , "  - Remove from ~/.ssh/config"
                , "  - Remove from /var/root/.ssh/*"
                , "  - Remove from /etc/nix/nix.custom.conf"
                , "  - Restart nix-daemon"
                ]
        system "echo 'Would run: cd builder && tofu destroy -auto-approve'"
        system "echo ''"
        system "echo 'Then remove config blocks between markers using awk'"

    slide "Final Verification" $ do
        note $
            unlines
                [ "After destroy, verify:"
                , ""
                , "  - Status shows no VMs"
                , "  - SSH config clean"
                , "  - Nix config clean"
                , ""
                , "Check GCP console: VMs should be gone!"
                ]
        system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"
        system "echo ''"
        system "grep 'nix-builder' ~/.ssh/config 2>/dev/null && echo 'SSH config: HAS ENTRY' || echo 'SSH config: CLEAN'"
        system "grep 'PANHARMONICON' /etc/nix/nix.custom.conf 2>/dev/null && echo 'Nix config: HAS ENTRY' || echo 'Nix config: CLEAN'"

    slide "Thank You!" $ do
        note $
            unlines
                [ "One command to deploy. One command to destroy."
                , ""
                , "Works from any MacBook."
                , "Each developer is isolated."
                , "All config changes tracked and reversible."
                , ""
                , "Questions?"
                ]
        system "echo ''"
        system "echo '  ╔══════════════════════════════════════════════════════════════╗'"
        system "echo '  ║           Panharmonicon Deployment Deep Dive                  ║'"
        system "echo '  ║                                                               ║'"
        system "echo '  ║   ./deploy.sh apply   - deploy everything                     ║'"
        system "echo '  ║   ./deploy.sh destroy - remove everything                     ║'"
        system "echo '  ║   ./deploy.sh status  - check current state                   ║'"
        system "echo '  ║                                                               ║'"
        system "echo '  ╚══════════════════════════════════════════════════════════════╝'"
        system "echo ''"
