/*
 * Minimal planets canvas web component for Horizons demo
 * Displays solar system in static 2D horizontal line layout
 */
class PlanetsCanvas extends HTMLElement {
    constructor() {
        super();
        this.canvas = document.createElement('canvas');
        this.appendChild(this.canvas);
        this.ctx = this.canvas.getContext('2d');

        // Hardcoded planet data matching backend (inner planets spaced, outer realistic-ish)
        this.planets = [
            {name: "Sun", x: -30, y: 125, radius: 100, color: "#FFFF00"},
            {name: "Mercury", x: 86, y: 125, radius: 6, color: "#8C7853"},
            {name: "Venus", x: 114, y: 125, radius: 12, color: "#FFC649"},
            {name: "Earth", x: 149, y: 125, radius: 13, color: "#4169E1"},
            {name: "Mars", x: 180, y: 125, radius: 8, color: "#CD5C5C"},
            {name: "Jupiter", x: 236, y: 125, radius: 35, color: "#DAA520"},
            {name: "Saturn", x: 375, y: 125, radius: 30, color: "#F4A460"},
            {name: "Uranus", x: 684, y: 125, radius: 18, color: "#4FD0E0"},
            {name: "Neptune", x: 1032, y: 125, radius: 18, color: "#4166F5"}
        ];

        this.selectedPlanet = null;

        // Setup event handlers
        this.setupClick();
        this.setupResize();

        // Initial draw
        this.resize();
    }

    setupResize() {
        // Resize observer to handle container size changes
        new ResizeObserver(() => this.resize()).observe(this);
    }

    resize() {
        const rect = this.getBoundingClientRect();
        this.canvas.width = rect.width;
        this.canvas.height = rect.height;
        this.draw();
    }

    setupClick() {
        this.canvas.addEventListener('click', (e) => {
            const rect = this.canvas.getBoundingClientRect();
            const mouseX = e.clientX - rect.left;
            const mouseY = e.clientY - rect.top;

            // Check if clicked on a planet
            let clickedPlanet = null;
            for (const planet of this.planets) {
                const dx = mouseX - planet.x;
                const dy = mouseY - planet.y;
                const distance = Math.sqrt(dx * dx + dy * dy);

                // Click tolerance: planet radius + 10px
                if (distance < planet.radius + 10) {
                    clickedPlanet = planet.name;
                    break;
                }
            }

            // Update selection and emit event
            this.selectedPlanet = clickedPlanet;
            this.draw();

            this.dispatchEvent(new CustomEvent('planetclick', {
                detail: { name: clickedPlanet },
                bubbles: true
            }));
        });
    }

    draw() {
        const ctx = this.ctx;
        const width = this.canvas.width;
        const height = this.canvas.height;

        // Clear canvas with black background
        ctx.fillStyle = '#000';
        ctx.fillRect(0, 0, width, height);

        // Draw Sun with glow (partially visible on left edge)
        const sun = this.planets[0];
        ctx.shadowBlur = 50;
        ctx.shadowColor = sun.color;
        ctx.fillStyle = sun.color;
        ctx.beginPath();
        ctx.arc(sun.x, sun.y, sun.radius, 0, Math.PI * 2);
        ctx.fill();
        ctx.shadowBlur = 0;

        // Draw planets (skip sun which is index 0)
        for (let i = 1; i < this.planets.length; i++) {
            const planet = this.planets[i];
            const isSelected = planet.name === this.selectedPlanet;

            // Planet glow (stronger if selected)
            ctx.shadowBlur = isSelected ? 15 : 8;
            ctx.shadowColor = planet.color;

            // Draw planet circle
            ctx.fillStyle = planet.color;
            ctx.beginPath();
            ctx.arc(planet.x, planet.y, planet.radius, 0, Math.PI * 2);
            ctx.fill();

            // Selection indicator
            if (isSelected) {
                ctx.strokeStyle = '#0ff';
                ctx.lineWidth = 2;
                ctx.beginPath();
                ctx.arc(planet.x, planet.y, planet.radius + 5, 0, Math.PI * 2);
                ctx.stroke();
            }

            ctx.shadowBlur = 0;

            // Planet label
            ctx.fillStyle = planet.color;
            ctx.font = '12px monospace';
            const labelWidth = ctx.measureText(planet.name).width;
            ctx.fillText(planet.name, planet.x - labelWidth / 2, planet.y + planet.radius + 20);
        }
    }
}

customElements.define('planets-canvas', PlanetsCanvas);
