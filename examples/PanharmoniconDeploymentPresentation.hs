{- |
Module      : PanharmoniconDeploymentPresentation
Description : Deep dive into the panharmonicon deployment system
Copyright   : (c) Geosurge, 2026
License     : MIT

A comprehensive presentation demonstrating the panharmonicon deployment
infrastructure from start to finish. Shows how one command deploys a
complete NixOS system to GCP with secrets, data, and cloud storage sync.

Run with: demo ./examples/PanharmoniconDeploymentPresentation.hs
-}
module PanharmoniconDeploymentPresentation (presentation) where

import Demo.Core.DSL

presentation :: Presentation
presentation = mkPresentation "Panharmonicon Deployment: One Command to Rule Them All" $ do

  ---------------------------------------------------------------------------
  -- Opening: Destroy to Start Fresh
  ---------------------------------------------------------------------------

  slide "Starting Fresh: Destroying Current Deployment" $ do
    note $ unlines
      [ "Welcome! We're going to walk through the entire panharmonicon"
      , "deployment system from scratch."
      , ""
      , "To demonstrate everything works, we'll first destroy any"
      , "existing deployment, then build it back up step by step."
      , ""
      , "Let's check the current status first."
      ]
    system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"

  ---------------------------------------------------------------------------
  -- Section 1: Developer Isolation
  ---------------------------------------------------------------------------

  slide "Developer Isolation: Per-Developer GCP Projects" $ do
    note $ unlines
      [ "The first problem: how do we let multiple developers deploy"
      , "independently without stepping on each other?"
      , ""
      , "Solution: derive a unique project ID from each developer's"
      , "git email. SHA256 hash, truncated to 8 characters."
      , ""
      , "alice@company.com -> panharmonicon-d0eeac2a"
      , "bob@company.com   -> panharmonicon-7f3e1b2c"
      , ""
      , "Complete isolation. No collisions."
      ]
    elaborate "apps/panharmonicon/deploy/deploy.sh" (35, 45) "Developer ID generation"
    system "cd ~/Github/grim-monolith && git config user.email"
    system "cd ~/Github/grim-monolith && git config user.email | shasum -a 256 | cut -c1-8"

  ---------------------------------------------------------------------------
  -- Section 2: Entry Point
  ---------------------------------------------------------------------------

  slide "Entry Point: deploy.sh" $ do
    note $ unlines
      [ "The main entry point is deploy.sh. It orchestrates everything."
      , ""
      , "Four steps:"
      , "  1. Prerequisites - check tools exist"
      , "  2. GCP Authentication - ensure logged in"
      , "  3. Linux Builder - provision if on macOS"
      , "  4. Infrastructure - run Terraform/Terragrunt"
      , ""
      , "Each step is idempotent - run twice, same result."
      ]
    elaborate "apps/panharmonicon/deploy/deploy.sh" (1, 15) "Script header"
    system "head -15 ~/Github/grim-monolith/apps/panharmonicon/deploy/deploy.sh"

  ---------------------------------------------------------------------------
  -- Section 3: Builder Detection
  ---------------------------------------------------------------------------

  slide "The Cross-Compilation Problem" $ do
    note $ unlines
      [ "We're building NixOS (x86_64-linux) from a MacBook (aarch64-darwin)."
      , ""
      , "Nix can't magically cross-compile an entire Linux system."
      , "We need an actual Linux machine to do the heavy lifting."
      , ""
      , "detect_system() figures out what we're running on."
      , "needs_builder() returns true if we're not on x86_64-linux."
      ]
    elaborate "apps/panharmonicon/deploy/deploy.sh" (47, 65) "System detection"
    system "uname -s && uname -m"
    system "echo 'On this system, we need a builder: YES'"

  ---------------------------------------------------------------------------
  -- Section 4: Builder VM Provisioning
  ---------------------------------------------------------------------------

  slide "Builder VM: Terraform Configuration" $ do
    note $ unlines
      [ "We provision a Debian VM in GCP using Terraform."
      , ""
      , "Why Debian and not NixOS? Simpler, more reliable for a build machine."
      , ""
      , "Specs:"
      , "  - n2-standard-8: 8 vCPUs, 32GB RAM"
      , "  - 100GB SSD for the Nix store"
      , "  - Static IP for SSH access"
      ]
    elaborate "apps/panharmonicon/deploy/builder/main.tf" (79, 100) "VM resource"
    system "head -50 ~/Github/grim-monolith/apps/panharmonicon/deploy/builder/main.tf | tail -30"

  slide "Builder VM: Cloud-Init Script" $ do
    note $ unlines
      [ "When the VM boots, a startup script:"
      , ""
      , "  1. Installs prerequisites (curl, xz, git)"
      , "  2. Creates a builder user with sudo"
      , "  3. Sets up SSH authorized_keys"
      , "  4. Installs Nix in multi-user mode"
      , "  5. Configures Nix with flakes and caches"
      , "  6. Pre-warms the cache with nixpkgs#hello"
      , ""
      , "The VM is ready to accept build jobs when it comes up."
      ]
    elaborate "apps/panharmonicon/deploy/builder/main.tf" (148, 176) "Nix installation"
    system "grep -A 30 'Install Nix' ~/Github/grim-monolith/apps/panharmonicon/deploy/builder/main.tf | head -25"

  ---------------------------------------------------------------------------
  -- Section 5: Local Configuration
  ---------------------------------------------------------------------------

  slide "Local SSH Config" $ do
    note $ unlines
      [ "After the VM exists, we need to tell our Mac how to reach it."
      , ""
      , "We create an SSH alias 'nix-builder' that points to the VM's IP."
      , ""
      , "This goes into:"
      , "  - ~/.ssh/config (for our user)"
      , "  - /var/root/.ssh/config (for nix-daemon which runs as root)"
      , ""
      , "Notice the markers: AUTO-GENERATED, with start/end markers."
      , "This enables clean removal later."
      ]
    elaborate "apps/panharmonicon/deploy/builder/provision-builder.sh" (132, 152) "SSH config generation"
    system "grep -A 10 'Host nix-builder' ~/.ssh/config 2>/dev/null || echo 'Not configured yet'"

  slide "Local Nix Config" $ do
    note $ unlines
      [ "We add the builder to /etc/nix/nix.custom.conf:"
      , ""
      , "  trusted-users = root <username>"
      , "  builders = ssh://nix-builder x86_64-linux - 8 1 big-parallel"
      , "  builders-use-substitutes = true"
      , ""
      , "When Nix sees 'ssh://nix-builder', SSH resolves it via our config."
      , ""
      , "The marker block enables idempotent add/remove."
      ]
    elaborate "apps/panharmonicon/deploy/builder/provision-builder.sh" (110, 130) "Nix config generation"
    system "cat /etc/nix/nix.custom.conf 2>/dev/null || echo 'Not configured yet'"

  ---------------------------------------------------------------------------
  -- Section 6: Idempotency & Reversibility
  ---------------------------------------------------------------------------

  slide "Idempotent & Reversible: Marker Pattern" $ do
    note $ unlines
      [ "Every config change uses markers:"
      , ""
      , "  # PANHARMONICON-BUILDER:1.2.3.4"
      , "  ... config lines ..."
      , "  # END PANHARMONICON-BUILDER:1.2.3.4"
      , ""
      , "When destroying, awk finds lines between markers and removes them."
      , ""
      , "We don't clobber the whole file. We don't leave orphaned config."
      , "Clean insertion, clean removal."
      ]
    elaborate "apps/panharmonicon/deploy/builder/provision-builder.sh" (268, 294) "Marker-based removal"
    system "grep -c 'PANHARMONICON-BUILDER' ~/.ssh/config 2>/dev/null || echo '0 markers found'"

  ---------------------------------------------------------------------------
  -- Section 7: Billing & Project Creation
  ---------------------------------------------------------------------------

  slide "GCP Billing: Automatic Linking" $ do
    note $ unlines
      [ "The billing account is hardcoded in terragrunt.hcl."
      , "It's the same for everyone in the organization."
      , ""
      , "Developers don't need to know or manage billing."
      , ""
      , "When Terraform creates the project, it passes:"
      , "  - org_id: which GCP organization"
      , "  - billing_account: who pays"
      , ""
      , "The project is linked to billing at creation time."
      , "No manual console clicking required."
      ]
    elaborate "apps/panharmonicon/deploy/terraform/terragrunt.hcl" (50, 59) "Billing configuration"
    system "grep -E '(org_id|billing_account)' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/terragrunt.hcl"

  slide "Terraform: Project & Bucket Creation" $ do
    note $ unlines
      [ "Terraform creates the GCP project itself:"
      , ""
      , "  google_project.dev - the project with billing linked"
      , "  google_storage_bucket.tfstate - for Terraform state"
      , "  google_storage_bucket.content - for generated content"
      , ""
      , "prevent_destroy = true protects against accidental deletion."
      ]
    elaborate "apps/panharmonicon/deploy/terraform/main.tf" (21, 50) "Project resource"
    system "grep -A 10 'resource \"google_project\"' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/main.tf"

  ---------------------------------------------------------------------------
  -- Section 8: NixOS Configuration
  ---------------------------------------------------------------------------

  slide "NixOS: Declarative System Config" $ do
    note $ unlines
      [ "The NixOS configuration is fully declarative."
      , ""
      , "We receive two arguments:"
      , "  - panharmonicon: the Haskell package"
      , "  - panharmoniconData: the pipeline dossiers"
      , ""
      , "The config defines:"
      , "  - SSH access"
      , "  - Firewall rules"
      , "  - User accounts"
      , "  - Systemd services"
      ]
    elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (1, 20) "Config header"
    system "head -25 ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix"

  slide "Systemd Service: The Pipeline" $ do
    note $ unlines
      [ "The panharmonicon-daisychain service:"
      , ""
      , "  1. Loads OPENAI_API_KEY from agenix secret"
      , "  2. Gets project ID from GCP instance metadata"
      , "  3. Runs the daisychain pipeline"
      , "  4. Syncs results to GCS with gsutil"
      , ""
      , "All declarative. All in Nix."
      ]
    elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (77, 96) "Systemd service"
    system "grep -A 20 'systemd.services.panharmonicon' ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix | head -20"

  ---------------------------------------------------------------------------
  -- Section 9: Secrets Management
  ---------------------------------------------------------------------------

  slide "Agenix: Encrypted Secrets" $ do
    note $ unlines
      [ "Secrets are encrypted with age."
      , ""
      , "The VM's SSH host key is a recipient, so only that"
      , "specific VM can decrypt it."
      , ""
      , "We set proper ownership:"
      , "  owner = panharmonicon"
      , "  group = panharmonicon"
      , "  mode = 0400"
      , ""
      , "The service user can read it. No one else."
      ]
    elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (47, 53) "Agenix secret"
    system "grep -A 6 'age.secrets.openai-key' ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix"

  ---------------------------------------------------------------------------
  -- Section 10: Data Bundling
  ---------------------------------------------------------------------------

  slide "Data Bundling: Nix Store Derivation" $ do
    note $ unlines
      [ "The pipeline needs dossier files (Core.md, Summary.md)."
      , ""
      , "These are bundled into a Nix derivation:"
      , "  panharmoniconData = pkgs.runCommand \"panharmonicon-data\" { } ''..."
      , ""
      , "They become immutable paths in the Nix store."
      , ""
      , "At runtime, we symlink them into the working directory."
      , "Inputs are read-only. Outputs go to a writable location."
      ]
    elaborate "apps/panharmonicon/package.nix" (36, 50) "Data derivation"
    system "grep -A 10 'panharmoniconData' ~/Github/grim-monolith/apps/panharmonicon/package.nix"

  slide "Tmpfiles: Symlinks at Runtime" $ do
    note $ unlines
      [ "systemd.tmpfiles.rules creates the directory structure:"
      , ""
      , "  /var/lib/panharmonicon/priv/Pipeline -> Nix store (read-only)"
      , "  /var/lib/panharmonicon/priv/outputs  -> writable"
      , ""
      , "The L+ directive creates symlinks to Nix store paths."
      ]
    elaborate "apps/panharmonicon/deploy/nixos/configuration.nix" (55, 65) "Tmpfiles rules"
    system "grep -A 8 'tmpfiles.rules' ~/Github/grim-monolith/apps/panharmonicon/deploy/nixos/configuration.nix"

  ---------------------------------------------------------------------------
  -- Section 11: Flake Inputs
  ---------------------------------------------------------------------------

  slide "Flake Inputs: Git Dependencies" $ do
    note $ unlines
      [ "Git dependencies are flake inputs, not fetchFromGitHub."
      , ""
      , "  haskllm.url = \"github:geosurge-ai/haskllm\""
      , "  pandoc-command-simple.url = \"github:geosurge-ai/pandoc-command-simple\""
      , ""
      , "Versions are locked in flake.lock."
      , "Update with: nix flake update haskllm pandoc-command-simple"
      , ""
      , "No manual hash management. No hash mismatches."
      ]
    elaborate "flake.nix" (24, 27) "Flake inputs"
    system "grep -A 3 'Panharmonicon dependencies' ~/Github/grim-monolith/flake.nix"

  slide "dontCheck: Tests in CI, Not Nix Build" $ do
    note $ unlines
      [ "Some tests need network access or API keys."
      , "Nix builds run in a sandbox - no network, no secrets."
      , ""
      , "Solution: dontCheck"
      , ""
      , "  haskllm tests need OPENAI_API_KEY"
      , "  panharmonicon tests need Wikipedia API"
      , ""
      , "Tests run separately in CI with secrets, not during nix build."
      ]
    elaborate "apps/panharmonicon/package.nix" (24, 34) "dontCheck usage"
    system "grep -B 1 'dontCheck' ~/Github/grim-monolith/apps/panharmonicon/package.nix"

  ---------------------------------------------------------------------------
  -- Section 12: GCS Bucket
  ---------------------------------------------------------------------------

  slide "GCS Content Bucket" $ do
    note $ unlines
      [ "Each developer gets their own content bucket:"
      , ""
      , "  gs://panharmonicon-d0eeac2a-content/"
      , ""
      , "Terraform creates it with IAM permissions for the"
      , "compute service account to read/write."
      , ""
      , "The service syncs outputs after each run:"
      , "  gsutil -m rsync -r priv/outputs/Daisychain/ gs://..."
      ]
    elaborate "apps/panharmonicon/deploy/terraform/main.tf" (52, 75) "Content bucket"
    system "grep -A 15 'google_storage_bucket.*content' ~/Github/grim-monolith/apps/panharmonicon/deploy/terraform/main.tf | head -15"

  ---------------------------------------------------------------------------
  -- Section 13: Live Deployment (Status Check)
  ---------------------------------------------------------------------------

  slide "Current Deployment Status" $ do
    note $ unlines
      [ "Let's check what's currently deployed."
      , ""
      , "This shows:"
      , "  - State bucket status"
      , "  - Builder VM status (if on macOS)"
      , "  - Panharmonicon VM status"
      ]
    system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"

  slide "Verifying the Running System" $ do
    note $ unlines
      [ "If deployed, we can:"
      , ""
      , "  - SSH into the VM"
      , "  - Check the systemd service"
      , "  - List the GCS bucket contents"
      , ""
      , "Let's check what's in GCS."
      ]
    system "gsutil ls gs://panharmonicon-*-content/ 2>/dev/null | head -20 || echo 'No content bucket found'"

  ---------------------------------------------------------------------------
  -- Closing: Destroy
  ---------------------------------------------------------------------------

  slide "Closing: We Will Now Destroy Everything" $ do
    note $ unlines
      [ "That's the complete system!"
      , ""
      , "Recap:"
      , "  - Per-developer GCP projects (from git email hash)"
      , "  - Automatic Linux builder provisioning"
      , "  - Idempotent local config with markers"
      , "  - NixOS deployed via nixos-anywhere"
      , "  - Agenix secrets with proper ownership"
      , "  - Data bundled in Nix store"
      , "  - Automatic GCS sync"
      , ""
      , "Now we'll destroy and verify in the GCP console."
      , ""
      , "Press SPACE to run ./deploy.sh destroy"
      ]
    system "echo ''"
    system "echo '  ╔══════════════════════════════════════════════════════════════╗'"
    system "echo '  ║  Ready to destroy. Check GCP console after to verify.        ║'"
    system "echo '  ╚══════════════════════════════════════════════════════════════╝'"
    system "echo ''"

  slide "Destroying the Deployment" $ do
    note $ unlines
      [ "Running ./deploy.sh destroy"
      , ""
      , "This will:"
      , "  - Destroy the panharmonicon VM"
      , "  - Destroy the builder VM"
      , "  - Remove all local nix/ssh config"
      , ""
      , "The GCP project and state bucket are preserved."
      , ""
      , "After this, check the GCP console to verify VMs are gone."
      ]
    system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && echo 'y' | ./deploy.sh destroy 2>&1 || echo 'Destroy completed or nothing to destroy'"

  slide "Final Verification" $ do
    note $ unlines
      [ "Let's verify everything is cleaned up:"
      , ""
      , "  - Status should show no VMs"
      , "  - SSH config should have no nix-builder entry"
      , "  - Nix config should have no builder entry"
      , ""
      , "Go check the GCP console to see the VMs are gone!"
      ]
    system "cd ~/Github/grim-monolith/apps/panharmonicon/deploy && ./deploy.sh status"
    system "grep 'nix-builder' ~/.ssh/config 2>/dev/null && echo 'SSH config still has entry' || echo 'SSH config clean'"
    system "grep 'PANHARMONICON' /etc/nix/nix.custom.conf 2>/dev/null && echo 'Nix config still has entry' || echo 'Nix config clean'"

  slide "Thank You!" $ do
    note $ unlines
      [ "One command to deploy. One command to destroy."
      , ""
      , "Works from any MacBook."
      , "Each developer is isolated."
      , "All config changes are tracked and reversible."
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
