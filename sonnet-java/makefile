BUILD_PATH = ./src
OUT_DIR    = ./build
JAR_FILE   = ./build/app.jar


SRCS := $(shell find $(BUILD_PATH) -name "*.java")

GREEN = \033[0;32m
RESET = \033[0m

all: build


format:
	jfmt write --config-file format.xml $(SRCS)
	@echo "$(GREEN)[Build] Alle Dateien erfolgreich formatiert.$(RESET)"

build: format  $(SRCS)
	@mkdir -p $(OUT_DIR)
	javac --release 25 -d $(OUT_DIR) $(SRCS)
	@echo "$(GREEN)[Build] Kompilierung abgeschlossen. Artifkate befinden sich in $(OUT_DIR).$(RESET)"

jar: build
	jar cfe $(JAR_FILE) app.App -C $(OUT_DIR) .
	@echo "$(GREEN)[Build] JAR-Datei erfolgreich erstellt in $(JAR_FILE).$(RESET)"


run: build
	@echo "$(GREEN)[Build] App wird ausgeführt:$(RESET)"
	@printf "$(GREEN)%.0s─$(RESET)" {1..80}; echo ""
	java -cp $(OUT_DIR) app.App $(args)


clean:
	rm -rf $(OUT_DIR)
	@echo "$(GREEN)[Build] Erfolgreich aufgeräumt.$(RESET)"