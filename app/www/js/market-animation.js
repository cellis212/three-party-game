'use strict';

(function (window, document, Shiny, gsap, PIXI) {
  if (!window || !document || !gsap || !PIXI) {
    return;
  }

  const MESSAGE_TYPE = 'market-sim-update';
  const INSTANCES = new Map();
  const MAX_ATTACH_ATTEMPTS = 10;

  function hexToInt(color) {
    if (typeof color !== 'string') {
      return 0xcbd5f5;
    }
    const trimmed = color.trim();
    if (!trimmed.length) {
      return 0xcbd5f5;
    }
    let hex = trimmed.replace('#', '');
    if (hex.length === 3) {
      hex = hex.split('').map((ch) => ch + ch).join('');
    }
    const parsed = parseInt(hex, 16);
    return Number.isFinite(parsed) ? parsed : 0xcbd5f5;
  }

  function formatCurrency(value) {
    if (typeof value !== 'number' || !Number.isFinite(value)) {
      return '$0';
    }
    const sign = value < 0 ? '-' : '';
    const abs = Math.abs(value);
    return `${sign}$${Math.round(abs).toLocaleString(undefined, { maximumFractionDigits: 0 })}`;
  }

  function clamp(value, min, max) {
    return Math.min(Math.max(value, min), max);
  }

  function randomBetween(min, max) {
    return min + Math.random() * (max - min);
  }

  function formatClock(current, total) {
    const formatPart = (seconds) => {
      const safe = Math.max(0, seconds);
      const mins = Math.floor(safe / 60);
      const secs = Math.floor(safe % 60);
      return `${String(mins).padStart(2, '0')}:${String(secs).padStart(2, '0')}`;
    };
    return `${formatPart(current)} / ${formatPart(total)}`;
  }

  function jitterPoint(base, radius) {
    const angle = Math.random() * Math.PI * 2;
    const distance = Math.random() * radius;
    return {
      x: base.x + Math.cos(angle) * distance,
      y: base.y + Math.sin(angle) * distance
    };
  }

  function truncateLabel(text, maxLength) {
    if (text === undefined || text === null) {
      return '';
    }
    const str = String(text);
    if (!Number.isFinite(maxLength) || maxLength <= 0) {
      return str;
    }
    if (str.length <= maxLength) {
      return str;
    }
    if (maxLength <= 3) {
      return str.slice(0, maxLength);
    }
    return `${str.slice(0, maxLength - 3)}...`;
  }

  function ensureElement(id) {
    return document.getElementById(id);
  }

  class MarketSimulationStoryboard {
    constructor(root, nsPrefix) {
      this.root = root;
      this.nsPrefix = nsPrefix || '';
      this.canvasHost = root.querySelector('[data-role="canvas-host"]');
      this.panelLabelsEl = root.querySelector('.market-sim-panel-labels');
      this.timelineProgressEl = root.querySelector('.market-sim-timeline-progress');
      this.timelineMarkerEl = root.querySelector('.market-sim-timeline-marker');
      this.captionEl = ensureElement(`${this.nsPrefix}market_sim_event_caption`);
      this.clockEl = ensureElement(`${this.nsPrefix}market_sim_clock`);
      this.controls = {
        play: ensureElement(`${this.nsPrefix}market_sim_play`),
        restart: ensureElement(`${this.nsPrefix}market_sim_restart`),
        skip: ensureElement(`${this.nsPrefix}market_sim_skip`),
        speed1: ensureElement(`${this.nsPrefix}market_sim_speed_1x`),
        speed2: ensureElement(`${this.nsPrefix}market_sim_speed_2x`),
        mute: ensureElement(`${this.nsPrefix}market_sim_mute`)
      };

      this.state = {
        playing: false,
        speed: 1
      };

      this.audioEnabled = true;
      this.audioCtx = null;
      this.currentPayload = null;
      this.timelineDuration = 120;
      this.timeline = null;
      this.layout = null;
      this.tokens = [];
      this.cashBars = [];
      this.cashBarContainer = null;
      this.circle = null;
      this.eventBook = [];
      this.stageGroupHeadings = [];
      this.circleHeading = null;
      this.groupHeadingStyle = null;
      this.annotationEl = ensureElement(`${this.nsPrefix}market_sim_annotation`);
      this.annotationTimeout = null;
      this.particlePool = [];
      this.activeParticles = [];

      this.initPixi();
      this.setupControls();
      this.resetScene();
      this.updatePlayButton();
      this.updateTimelineUI(0);
      this.updateMuteButton();
    }

    initPixi() {
      if (!this.canvasHost) {
        throw new Error('Market simulation canvas host missing');
      }

      this.canvasHost.innerHTML = '';
      const rect = this.canvasHost.getBoundingClientRect();
      const initWidth = rect.width > 0 ? rect.width : 960;
      const initHeight = rect.height > 0 ? rect.height : 640;
      this.app = new PIXI.Application({
        width: initWidth,
        height: initHeight,
        backgroundAlpha: 0,
        antialias: true,
        autoDensity: true,
        resolution: (window.devicePixelRatio || 1)
      });
      this.canvasHost.appendChild(this.app.view);
      this.app.view.style.width = '100%';
      this.app.view.style.height = '100%';

      this.layers = {
        backdrop: new PIXI.Container(),
        bars: new PIXI.Container(),
        tokens: new PIXI.Container(),
        overlays: new PIXI.Container()
      };

      this.app.stage.addChild(this.layers.backdrop);
      this.app.stage.addChild(this.layers.bars);
      this.app.stage.addChild(this.layers.tokens);
      this.app.stage.addChild(this.layers.overlays);
    }

    setupControls() {
      const playHandler = (evt) => {
        evt.preventDefault();
        this.ensureAudio();
        if (this.state.playing) {
          this.pause();
        } else {
          this.play();
        }
      };

      const restartHandler = (evt) => {
        evt.preventDefault();
        this.ensureAudio();
        if (this.timeline) {
          this.timeline.seek(0);
          this.pause(false);
          this.play();
        }
      };

      const skipHandler = (evt) => {
        evt.preventDefault();
        this.ensureAudio();
        this.skipToNextPhase();
      };

      const speedHandler = (speed) => (evt) => {
        evt.preventDefault();
        this.ensureAudio();
        this.setSpeed(speed);
      };

      const muteHandler = (evt) => {
        evt.preventDefault();
        this.toggleAudio();
      };

      if (this.controls.play) {
        this.controls.play.addEventListener('click', playHandler);
      }
      if (this.controls.restart) {
        this.controls.restart.addEventListener('click', restartHandler);
      }
      if (this.controls.skip) {
        this.controls.skip.addEventListener('click', skipHandler);
      }
      if (this.controls.speed1) {
        this.controls.speed1.addEventListener('click', speedHandler(1));
      }
      if (this.controls.speed2) {
        this.controls.speed2.addEventListener('click', speedHandler(2));
      }
      if (this.controls.mute) {
        this.controls.mute.addEventListener('click', muteHandler);
      }
    }

    resetScene() {
      if (!this.app) {
        return;
      }
      this.layers.backdrop.removeChildren();
      this.layers.bars.removeChildren();
      this.circle = null;
      this.tokens = [];
      this.cashBars = [];
      this.cashBarContainer = null;
      this.insurerBubbles = [];
      this.hospitalBubbles = [];
      this.routingFlows = [];
      this.particlePool = [];
      this.activeParticles = [];
      if (this.layers.tokens) {
        this.layers.tokens.removeChildren();
      }
      if (this.layers.overlays) {
        this.layers.overlays.removeChildren();
      }
      if (this.timeline) {
        this.timeline.kill();
        this.timeline = null;
      }
      if (this.annotationTimeout) {
        clearTimeout(this.annotationTimeout);
        this.annotationTimeout = null;
      }
      if (this.annotationEl) {
        this.annotationEl.textContent = '';
        this.annotationEl.classList.remove('visible');
      }
    }

    ensureAudio() {
      if (!this.audioEnabled) {
        return null;
      }
      if (this.audioCtx) {
        if (this.audioCtx.state === 'suspended') {
          this.audioCtx.resume();
        }
        return this.audioCtx;
      }
      const Ctx = window.AudioContext || window.webkitAudioContext;
      if (!Ctx) {
        return null;
      }
      this.audioCtx = new Ctx();
      return this.audioCtx;
    }

    toggleAudio() {
      this.audioEnabled = !this.audioEnabled;
      if (!this.audioEnabled && this.audioCtx && this.audioCtx.state !== 'closed') {
        this.audioCtx.suspend();
      }
      this.updateMuteButton();
    }

    updateMuteButton() {
      const button = this.controls.mute;
      if (!button) {
        return;
      }
      const icon = button.querySelector('i');
      if (icon) {
        icon.classList.remove('fa-volume-up', 'fa-volume-mute');
        icon.classList.add(this.audioEnabled ? 'fa-volume-up' : 'fa-volume-mute');
      }
    }

    playTone(frequency, duration) {
      if (!this.audioEnabled) {
        return;
      }
      const ctx = this.ensureAudio();
      if (!ctx) {
        return;
      }
      const osc = ctx.createOscillator();
      const gain = ctx.createGain();
      osc.type = 'sine';
      osc.frequency.value = frequency;
      gain.gain.value = 0.0001;
      osc.connect(gain);
      gain.connect(ctx.destination);
      const now = ctx.currentTime;
      gain.gain.setValueAtTime(0.02, now);
      gain.gain.exponentialRampToValueAtTime(0.0001, now + duration);
      osc.start(now);
      osc.stop(now + duration + 0.05);
    }

    playBounceSound() {
      this.playTone(220 + Math.random() * 40, 0.25);
    }

    playDepositSound() {
      this.playTone(520 + Math.random() * 80, 0.15);
    }

    loadPayload(payload, options) {
      const opts = options || {};
      if (!payload || !payload.people || !payload.people.length) {
        this.currentPayload = null;
        this.resetScene();
        this.updateTimelineUI(0);
        if (this.captionEl) {
          this.captionEl.textContent = 'Awaiting data...';
        }
        return;
      }

      const targetTime = Number.isFinite(opts.initialTime) ? opts.initialTime : 0;
      const resumePlaying = Boolean(opts.resumePlaying);

      this.currentPayload = payload;
      this.resetScene();

      // Always re-measure canvas host and resize PIXI renderer
      if (this.app && this.canvasHost) {
        const rect = this.canvasHost.getBoundingClientRect();
        if (rect.width > 0 && rect.height > 0) {
          this.app.renderer.resize(rect.width, rect.height);
        }
      }

      this.configureLayout(payload);
      this.drawBackground(payload);
      this.buildCircleCity(payload);
      this.initParticlePool();
      this.buildRoutingFlows(payload);
      this.buildCashPositionBars(payload);
      this.timelineDuration = payload.timeline && payload.timeline.totalSeconds ? payload.timeline.totalSeconds : 120;
      this.buildTimeline(payload);
      this.positionGroupHeadings();
      this.positionExternalPanelLabels();

      if (targetTime > 0 && this.timelineDuration > 0) {
        const clamped = clamp(targetTime, 0, this.timelineDuration);
        this.timeline.pause(clamped);
        this.timeline.seek(clamped, false);
        this.updateTimelineUI(clamped);
      } else {
        this.updateTimelineUI(0);
      }

      const shouldAutoPlay = opts.autoPlay === undefined ? true : opts.autoPlay;
      if (resumePlaying) {
        this.play();
      } else if (shouldAutoPlay) {
        this.play();
      } else {
        this.pause(false);
      }
    }

    configureLayout(payload) {
      const bounds = this.canvasHost.getBoundingClientRect();
      const width = bounds.width || 960;
      const height = bounds.height || 540;
      const marginX = 48;
      
      // Cap circle radius so stage area + persistent bars fit below
      const reservedBelow = 400; // stage bars + persistent insurer bars + margins
      const maxCircleRadius = Math.max(80, (height - reservedBelow - 120 - 40 - 36 - 60) / 2);
      const circleRadius = Math.max(80, Math.min(Math.min(width, height) * 0.35, maxCircleRadius));
      const circleCenterX = width / 2;
      
      // Simplify: Just ensure enough top margin for heading + clearance above labels
      // Labels extend (circleRadius + 36) from circle center vertically
      // Top labels will be at: circleCenterY - circleRadius - 36
      // We want heading bottom well above that
      const marginY = 120; // Fixed generous top margin to prevent all overlaps
      const circleCenterY = marginY + circleRadius + 40; // Position circle lower to accommodate everything
      const bottomMargin = 80; // extra space for labels below bars

      // Stage area starts after circle labels and group headings
      const labelBottom = circleCenterY + circleRadius + 36;
      const groupHeadingHeight = 25; // approximate
      const stageTop = labelBottom + 20 + groupHeadingHeight + 15; // labels + clearance + heading + more clearance
      const cashBarHeight = Math.max(200, height - stageTop - bottomMargin);
      const cashBarArea = {
        x: marginX,
        y: stageTop,
        width: Math.max(240, width - marginX * 2),
        height: cashBarHeight
      };

      this.layout = {
        width,
        height,
        marginX,
        marginY,
        circle: {
          centerX: circleCenterX,
          centerY: circleCenterY,
          radius: circleRadius
        },
        stageArea: cashBarArea,
        cashBarArea
      };
    }

    drawBackground(payload) {
      if (!this.layout) {
        return;
      }

      this.layers.backdrop.removeChildren();

      const circle = this.layout.circle;
      const stageArea = this.layout.stageArea;

      const circleBackdrop = new PIXI.Graphics();
      circleBackdrop.lineStyle(2, 0x1f3b82, 0.55);
      circleBackdrop.beginFill(0x0b132b, 0.55);
      circleBackdrop.drawCircle(circle.centerX, circle.centerY, circle.radius + 18);
      circleBackdrop.endFill();
      this.layers.backdrop.addChild(circleBackdrop);

      const cashArea = this.layout.cashBarArea;
      const cashBackground = new PIXI.Graphics();
      cashBackground.lineStyle(0);
      cashBackground.beginFill(0x0b132b, 0.75);
      cashBackground.drawRoundedRect(cashArea.x - 18, cashArea.y - 24, cashArea.width + 36, cashArea.height + 48, 28);
      cashBackground.endFill();
      this.layers.backdrop.addChild(cashBackground);

      const headingStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: 16,
        fill: 0xf1f5f9, // slightly brighter for better contrast
        fontWeight: '700',
        dropShadow: true,
        dropShadowColor: 0x0b132b,
        dropShadowBlur: 2,
        dropShadowDistance: 1
      });

      const circleHeading = new PIXI.Text('Circle City Market', headingStyle);
      circleHeading.anchor.set(0.5, 1);
      circleHeading.x = circle.centerX;
      // Position heading at top with proper clearance
      circleHeading.y = this.layout.marginY - 10; // 10px from top
      this.layers.backdrop.addChild(circleHeading);
      this.circleHeading = circleHeading;

      this.layers.backdrop.cacheAsBitmap = true;
    }

    buildCircleCity(payload) {
      if (!this.layout) {
        return;
      }
      if (this.layers.overlays) {
        this.layers.overlays.removeChildren();
      }
      
      // Prefer the DOM-based panel labels when present to avoid duplication
      if (this.panelLabelsEl) {
        this.stageGroupHeadings = [];
      } else {
        // Create PIXI group headings only when DOM labels are not available
        const groupHeadingStyle = new PIXI.TextStyle({
          fontFamily: 'Segoe UI, sans-serif',
          fontSize: 14,
          fill: 0xf8fafc,
          fontWeight: '700',
          letterSpacing: 1.2
        });
        const groupHeadingTexts = ['Insurance Choice', 'Hospital Routing', 'Profit / Loss'];
        this.stageGroupHeadings = groupHeadingTexts.map((labelText) => {
          const label = new PIXI.Text(labelText.toUpperCase(), groupHeadingStyle);
          label.anchor.set(0.5, 0);
          this.layers.overlays.addChild(label);
          return label;
        });
      }
      
      const circle = this.layout.circle;
      const container = new PIXI.Container();
      container.name = 'circle-city';
      this.layers.overlays.addChild(container);

      const outerRing = new PIXI.Graphics();
      outerRing.lineStyle(4, 0x1e3a8a, 0.85);
      outerRing.drawCircle(circle.centerX, circle.centerY, circle.radius);
      container.addChild(outerRing);

      const innerFill = new PIXI.Graphics();
      innerFill.beginFill(0x0f172a, 0.65);
      innerFill.drawCircle(circle.centerX, circle.centerY, Math.max(20, circle.radius - 24));
      innerFill.endFill();
      container.addChild(innerFill);

      const hub = new PIXI.Graphics();
      hub.beginFill(0x0ea5e9, 0.35);
      hub.drawCircle(circle.centerX, circle.centerY, 16);
      hub.endFill();
      container.addChild(hub);

      // Insurer bubbles (inside circle) + Hospital bubbles (on ring)
      const insurers = payload.insurers || [];
      const people = payload.people || [];
      this.insurerBubbles = [];
      this.hospitalBubbles = [];

      if (insurers.length > 0) {
        // Count sick per insurer category
        const sickByInsurer = new Map();
        let medicareCount = 0;
        let uninsuredCount = 0;
        let medicareSick = 0;
        let uninsuredSick = 0;
        people.forEach((p) => {
          if (!p) { return; }
          if (p.insurerCategory === 'medicare') {
            medicareCount += 1;
            if (p.sick) { medicareSick += 1; }
          } else if (p.insurerCategory === 'uninsured') {
            uninsuredCount += 1;
            if (p.sick) { uninsuredSick += 1; }
          } else if (p.insurerCategory === 'private' && p.insurerId) {
            if (p.sick) {
              sickByInsurer.set(p.insurerId, (sickByInsurer.get(p.insurerId) || 0) + 1);
            }
          }
        });

        const ringBubbles = insurers.map((ins) => ({
          id: ins.insurer_id,
          label: truncateLabel(ins.insurerShort || ins.insurer_name || `Insurer ${ins.insurer_id}`, 14),
          color: ins.insurer_color || '#94a3b8',
          enrollees: Number(ins.total_enrollees) || 0,
          sickCount: sickByInsurer.get(ins.insurer_id) || 0,
          category: 'private'
        }));
        ringBubbles.push({ id: 'medicare', label: 'Medicare', color: '#38bdf8', enrollees: medicareCount, sickCount: medicareSick, category: 'medicare' });

        const uninsuredBubble = { id: 'uninsured', label: 'Uninsured', color: '#f87171', enrollees: uninsuredCount, sickCount: uninsuredSick, category: 'uninsured' };
        const allBubbles = ringBubbles.concat([uninsuredBubble]);
        const maxEnrollees = Math.max(1, ...allBubbles.map((b) => b.enrollees));
        const maxBubbleRadius = Math.max(16, circle.radius * 0.28);
        const minBubbleRadius = 8;
        const innerRadius = circle.radius * 0.5;

        const nameStyle = new PIXI.TextStyle({
          fontFamily: 'Segoe UI, sans-serif', fontSize: 9, fill: 0xffffff, fontWeight: '700', align: 'center'
        });
        const countStyle = new PIXI.TextStyle({
          fontFamily: 'Segoe UI, sans-serif', fontSize: 11, fill: 0xffffff, fontWeight: '800'
        });

        const addBubble = (item, bx, by) => {
          const finalRadius = minBubbleRadius + (item.enrollees / maxEnrollees) * (maxBubbleRadius - minBubbleRadius);
          const shrunkRadius = minBubbleRadius + ((item.enrollees - item.sickCount) / maxEnrollees) * (maxBubbleRadius - minBubbleRadius);

          const bubbleGfx = new PIXI.Graphics();
          bubbleGfx.x = bx;
          bubbleGfx.y = by;
          container.addChild(bubbleGfx);

          const nameLabel = new PIXI.Text(item.label, nameStyle);
          nameLabel.anchor.set(0.5, 0);
          nameLabel.x = bx;
          nameLabel.y = by + finalRadius + 4;
          nameLabel.alpha = 0;
          container.addChild(nameLabel);

          const countLabel = new PIXI.Text('0', countStyle);
          countLabel.anchor.set(0.5, 0.5);
          countLabel.x = bx;
          countLabel.y = by;
          countLabel.alpha = 0;
          container.addChild(countLabel);

          const colorInt = hexToInt(item.color);
          bubbleGfx.beginFill(colorInt, 0.75);
          bubbleGfx.drawCircle(0, 0, 4);
          bubbleGfx.endFill();
          nameLabel.alpha = 0.6;

          this.insurerBubbles.push({
            item, gfx: bubbleGfx, nameLabel, countLabel,
            x: bx, y: by, finalRadius, shrunkRadius: Math.max(minBubbleRadius, shrunkRadius), color: colorInt
          });
        };

        const ringCount = ringBubbles.length;
        ringBubbles.forEach((item, idx) => {
          const angle = -Math.PI / 2 + (idx / ringCount) * Math.PI * 2;
          const bx = circle.centerX + Math.cos(angle) * innerRadius;
          const by = circle.centerY + Math.sin(angle) * innerRadius;
          addBubble(item, bx, by);
        });
        addBubble(uninsuredBubble, circle.centerX, circle.centerY);
      }

      const hospitals = (payload.hospitals && payload.hospitals.length ? payload.hospitals : [{
        hospital_id: 0,
        hospital_name: 'Hospital',
        hospital_color: '#60a5fa'
      }]);
      const count = hospitals.length || 1;
      const labelStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: 11,
        fill: 0xcbd5f5
      });

      const nodes = [];
      hospitals.forEach((hospital, index) => {
        const angle = -Math.PI / 2 + (index / count) * Math.PI * 2;
        const nodeX = circle.centerX + Math.cos(angle) * circle.radius;
        const nodeY = circle.centerY + Math.sin(angle) * circle.radius;

        const sprite = new PIXI.Graphics();
        sprite.beginFill(hexToInt(hospital.hospital_color || '#60a5fa'), 0.92);
        sprite.drawCircle(0, 0, 9);
        sprite.endFill();
        sprite.x = nodeX;
        sprite.y = nodeY;
        sprite.alpha = 0.32;
        container.addChild(sprite);

        const label = new PIXI.Text(truncateLabel(hospital.hospitalShort || hospital.hospital_name || `Hospital ${hospital.hospital_id}`, 20), labelStyle);
        label.anchor.set(0.5, 0.5);
        const labelRadius = circle.radius + 36; // add a bit more clearance from ring
        label.x = circle.centerX + Math.cos(angle) * labelRadius;
        label.y = circle.centerY + Math.sin(angle) * labelRadius;
        label.alpha = 0.7;
        container.addChild(label);

        nodes.push({
          id: hospital.hospital_id,
          angle,
          sprite,
          label,
          threshold: count ? (index + 1) / count : 1
        });
      });

      const pointer = new PIXI.Graphics();
      pointer.beginFill(0xfacc15, 0.95);
      pointer.drawCircle(0, 0, 7);
      pointer.endFill();
      pointer.x = circle.centerX;
      pointer.y = circle.centerY - circle.radius;
      pointer.alpha = 0;
      container.addChild(pointer);

      const tail = null;

      this.circle = {
        container,
        centerX: circle.centerX,
        centerY: circle.centerY,
        radius: circle.radius,
        pointer,
        tail,
        hospitalNodes: nodes,
        pointerProgress: 0
      };

      // Hospital bubbles — grow during routing phase as patients arrive
      const hospitalList = payload.hospitals || [];
      if (hospitalList.length > 0 && nodes.length > 0) {
        const maxPatients = Math.max(1, ...hospitalList.map((h) => Number(h.total_patients) || 0));
        const maxHospBubbleRadius = Math.max(14, circle.radius * 0.22);
        const minHospBubbleRadius = 9;

        const hospCountStyle = new PIXI.TextStyle({
          fontFamily: 'Segoe UI, sans-serif', fontSize: 10, fill: 0xffffff, fontWeight: '800'
        });

        hospitalList.forEach((hosp) => {
          const node = nodes.find((n) => n.id === hosp.hospital_id);
          if (!node) { return; }
          const patients = Number(hosp.total_patients) || 0;
          const finalRadius = minHospBubbleRadius + (patients / maxPatients) * (maxHospBubbleRadius - minHospBubbleRadius);

          const bubbleGfx = new PIXI.Graphics();
          bubbleGfx.x = node.sprite.x;
          bubbleGfx.y = node.sprite.y;
          container.addChild(bubbleGfx);

          const countLabel = new PIXI.Text('0', hospCountStyle);
          countLabel.anchor.set(0.5, 0.5);
          countLabel.x = node.sprite.x;
          countLabel.y = node.sprite.y;
          countLabel.alpha = 0;
          container.addChild(countLabel);

          this.hospitalBubbles.push({
            hospitalId: hosp.hospital_id,
            gfx: bubbleGfx, countLabel,
            x: node.sprite.x, y: node.sprite.y,
            finalRadius, patients,
            color: hexToInt(hosp.hospital_color || '#60a5fa')
          });
        });
      }

      this.resetCircleHighlights();
      this.updateCircleProgress(0);
      this.positionCircleHeading();
      this.positionGroupHeadings();
      this.positionExternalPanelLabels();
    }

    positionCircleHeading() {
      if (!this.circleHeading || !this.circle || !Array.isArray(this.circle.hospitalNodes)) {
        return;
      }

      const heading = this.circleHeading;
      const circle = this.circle;
      const defaultY = circle.centerY - circle.radius - 15;
      let topLabelTop = Number.POSITIVE_INFINITY;

      this.circle.hospitalNodes.forEach((node) => {
        if (!node || !node.label) {
          return;
        }
        const bounds = node.label.getBounds();
        if (Number.isFinite(bounds.top)) {
          topLabelTop = Math.min(topLabelTop, bounds.top);
        }
      });

      const padding = 10;
      if (Number.isFinite(topLabelTop)) {
        const adjusted = Math.min(defaultY, topLabelTop - padding);
        heading.y = Math.max(adjusted, 24);
      } else {
        heading.y = defaultY;
      }
    }

    positionGroupHeadings() {
      if (!this.stageGroupHeadings || !this.stageGroupHeadings.length || !this.layout) {
        return;
      }

      const { width, stageArea, circle } = this.layout;
      // Position headings just above the stage area with proper clearance
      const top = stageArea.y - 30; // positioned above stage area
      const columnWidth = width / this.stageGroupHeadings.length;

      this.stageGroupHeadings.forEach((heading, index) => {
        heading.x = columnWidth * (index + 0.5);
        heading.y = top;
      });
    }

    // Position DOM-based panel labels so they never overlap the PIXI heading;
    // keep them as high as possible (usually the stylesheet default ~16px),
    // but clamp lower if the PIXI title would collide.
    positionExternalPanelLabels() {
      if (!this.panelLabelsEl || !this.circleHeading || !this.layout) {
        return;
      }
      // Start with stylesheet default
      this.panelLabelsEl.style.top = '';

      const panelRect = this.panelLabelsEl.getBoundingClientRect();
      const canvasRect = this.canvasHost ? this.canvasHost.getBoundingClientRect() : null;
      const headingBounds = this.circleHeading.getBounds();
      if (!panelRect || !canvasRect || !headingBounds) {
        return; // fall back to CSS
      }

      // Convert heading top from global to stage-local coordinates
      const headingTopLocal = headingBounds.top - canvasRect.top;
      const panelHeight = panelRect.height || 0;
      const cssDefaultTop = 16; // matches stylesheet
      const minGap = 8; // minimum pixels between labels and title

      // Compute the highest allowed top so the bottom of the panel stays
      // at least minGap above the PIXI title top edge
      const maxTopWithoutOverlap = Math.max(0, Math.round(headingTopLocal - panelHeight - minGap));
      const finalTop = Math.min(cssDefaultTop, maxTopWithoutOverlap);
      this.panelLabelsEl.style.top = `${finalTop}px`;

      // If DOM labels exist, ensure we are not also rendering PIXI duplicates
      if (Array.isArray(this.stageGroupHeadings) && this.stageGroupHeadings.length) {
        this.stageGroupHeadings.forEach((g) => {
          try {
            if (g && g.parent) {
              g.parent.removeChild(g);
            }
          } catch (e) {
            // no-op
          }
        });
        this.stageGroupHeadings = [];
      }
    }

    // --- Cash Position Bar System ---

    computeCashPositionData(payload) {
      const insurers = (payload.insurers || []).map((ins) => {
        const premiumRevenue = Number(ins.premium_revenue) || 0;
        const claimsPaid = Number(ins.claims_paid) || 0;
        const adminCosts = Number(ins.administrative_costs) || 0;
        const profit = premiumRevenue - claimsPaid - adminCosts;
        return {
          key: `ins-${ins.insurer_id}`,
          label: ins.insurerShort || ins.insurer_name || `Insurer ${ins.insurer_id}`,
          color: ins.insurer_color || '#22c55e',
          type: 'insurer',
          startValue: -adminCosts,
          phase1End: premiumRevenue - adminCosts,
          phase2End: premiumRevenue - adminCosts,
          phase3End: profit,
          phase4End: profit
        };
      });

      const hospitals = (payload.hospitals || []).map((h) => {
        const costs = Number(h.costs) || 0;
        const networkCost = Number(h.network_contract_cost) || 0;
        const privateRevenue = Number(h.private_revenue) || 0;
        const publicRevenue = Number(h.public_revenue) || 0;
        const uninsuredRevenue = Number(h.uninsured_revenue) || 0;
        return {
          key: `hosp-${h.hospital_id}`,
          label: h.hospitalShort || h.hospital_name || `Hospital ${h.hospital_id}`,
          color: h.hospital_color || '#60a5fa',
          type: 'hospital',
          startValue: -networkCost,
          phase1End: -networkCost,
          phase2End: -costs,
          phase3End: -costs + privateRevenue,
          phase4End: -costs + privateRevenue + publicRevenue + uninsuredRevenue
        };
      });

      return insurers.concat(hospitals);
    }

    buildCashPositionBars(payload) {
      if (!this.layout || !this.layout.cashBarArea) {
        return;
      }
      this.layers.bars.removeChildren();

      const data = this.computeCashPositionData(payload);
      if (!data.length) {
        return;
      }

      const container = new PIXI.Container();
      container.name = 'cash-position-bars';
      this.layers.bars.addChild(container);
      this.cashBarContainer = container;

      const area = this.layout.cashBarArea;
      const insurerCount = data.filter((d) => d.type === 'insurer').length;
      const hospitalCount = data.filter((d) => d.type === 'hospital').length;
      const totalBars = data.length;
      const groupGap = Math.max(20, area.width * 0.04);

      // Responsive sizing
      const availableWidth = area.width - groupGap;
      const spacing = totalBars ? availableWidth / totalBars : availableWidth;
      const barWidth = Math.max(18, Math.min(70, spacing * 0.55));
      const labelTruncate = totalBars > 14 ? 8 : totalBars > 8 ? 12 : 18;
      const labelFontSize = totalBars > 14 ? 9 : totalBars > 8 ? 10 : 11;
      const valueFontSize = totalBars > 14 ? 10 : 12;

      // Zero line at vertical center of area, leaving room for labels
      const labelReserve = 40;
      const valueReserve = 24;
      const zeroY = area.y + valueReserve + (area.height - labelReserve - valueReserve) / 2;
      const maxBarHeight = Math.max(40, (area.height - labelReserve - valueReserve) / 2 - 10);

      // Compute global scale from all phase endpoints
      let maxAbsValue = 1;
      data.forEach((d) => {
        maxAbsValue = Math.max(maxAbsValue,
          Math.abs(d.startValue), Math.abs(d.phase1End),
          Math.abs(d.phase2End), Math.abs(d.phase3End),
          Math.abs(d.phase4End));
      });

      // Draw zero line (dashed)
      const zeroLine = new PIXI.Graphics();
      zeroLine.lineStyle(1.5, 0x475569, 0.7);
      const dashLen = 8;
      const gapLen = 6;
      for (let x = area.x; x < area.x + area.width; x += dashLen + gapLen) {
        zeroLine.moveTo(x, zeroY);
        zeroLine.lineTo(Math.min(x + dashLen, area.x + area.width), zeroY);
      }
      container.addChild(zeroLine);

      // Zero label
      const zeroLabelStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: 10,
        fill: 0x64748b,
        fontWeight: '500'
      });
      const zeroLabel = new PIXI.Text('$0', zeroLabelStyle);
      zeroLabel.anchor.set(1, 0.5);
      zeroLabel.x = area.x - 6;
      zeroLabel.y = zeroY;
      container.addChild(zeroLabel);

      // Group labels
      const groupLabelStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: 11,
        fill: 0x94a3b8,
        fontWeight: '600',
        letterSpacing: 1
      });

      const insurerGroupWidth = insurerCount * spacing;
      if (insurerCount > 0) {
        const insurerLabel = new PIXI.Text('INSURERS', groupLabelStyle);
        insurerLabel.anchor.set(0.5, 1);
        insurerLabel.x = area.x + insurerGroupWidth / 2;
        insurerLabel.y = area.y + 2;
        container.addChild(insurerLabel);
      }

      if (hospitalCount > 0) {
        const hospitalLabel = new PIXI.Text('HOSPITALS', groupLabelStyle);
        hospitalLabel.anchor.set(0.5, 1);
        hospitalLabel.x = area.x + insurerGroupWidth + groupGap + (hospitalCount * spacing) / 2;
        hospitalLabel.y = area.y + 2;
        container.addChild(hospitalLabel);
      }

      // Bar styles
      const nameLabelStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: labelFontSize,
        fill: 0xe2e8f0,
        align: 'center'
      });
      const valueLabelStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: valueFontSize,
        fill: 0xf8fafc,
        fontWeight: '600'
      });

      this.cashBars = data.map((item, index) => {
        const isHospital = item.type === 'hospital';
        const groupOffset = isHospital ? insurerCount * spacing + groupGap : 0;
        const localIndex = isHospital ? index - insurerCount : index;
        const centerX = area.x + groupOffset + spacing * (localIndex + 0.5);
        const color = hexToInt(item.color);

        const graphics = new PIXI.Graphics();
        container.addChild(graphics);

        const valueLabel = new PIXI.Text('$0', valueLabelStyle);
        valueLabel.anchor.set(0.5, 1);
        valueLabel.x = centerX;
        valueLabel.y = zeroY - 14;
        container.addChild(valueLabel);

        const nameLabel = new PIXI.Text(truncateLabel(item.label, labelTruncate), nameLabelStyle);
        nameLabel.anchor.set(0.5, 0);
        nameLabel.x = centerX;
        nameLabel.y = zeroY + maxBarHeight + 8;
        container.addChild(nameLabel);

        return {
          item,
          color,
          centerX,
          zeroY,
          barWidth,
          maxBarHeight,
          maxAbsValue,
          graphics,
          valueLabel,
          nameLabel,
          currentValue: 0
        };
      });

      // Draw initial state at starting value (negative admin/network costs)
      this.cashBars.forEach((bar) => this.redrawCashBar(bar, bar.item.startValue));
    }

    redrawCashBar(bar, value) {
      const ratio = bar.maxAbsValue ? clamp(Math.abs(value) / bar.maxAbsValue, 0, 1) : 0;
      const height = ratio * bar.maxBarHeight;
      const drawHeight = Math.max(2, height);
      const radius = Math.min(6, bar.barWidth / 3);

      bar.graphics.clear();
      bar.graphics.beginFill(bar.color, 0.88);

      if (value >= 0) {
        // Draw upward from zero line
        bar.graphics.drawRoundedRect(
          bar.centerX - bar.barWidth / 2,
          bar.zeroY - drawHeight,
          bar.barWidth,
          drawHeight,
          radius
        );
      } else {
        // Draw downward from zero line
        bar.graphics.drawRoundedRect(
          bar.centerX - bar.barWidth / 2,
          bar.zeroY,
          bar.barWidth,
          drawHeight,
          radius
        );
      }
      bar.graphics.endFill();

      bar.valueLabel.text = formatCurrency(Math.round(value));

      // Position value label above positive bars, below negative bars
      const minOffset = 14;
      if (value >= 0) {
        bar.valueLabel.anchor.set(0.5, 1);
        bar.valueLabel.y = bar.zeroY - Math.max(height, minOffset) - 4;
      } else {
        bar.valueLabel.anchor.set(0.5, 0);
        bar.valueLabel.y = bar.zeroY + Math.max(height, minOffset) + 4;
      }

      bar.currentValue = value;
    }

    updateCashBars(phase, progress) {
      if (!this.cashBars || !this.cashBars.length) {
        return;
      }
      const clamped = clamp(progress, 0, 1);

      this.cashBars.forEach((bar) => {
        const d = bar.item;
        let startVal, endVal;

        if (phase === 1) {
          startVal = d.startValue;
          endVal = d.phase1End;
        } else if (phase === 2) {
          startVal = d.phase1End;
          endVal = d.phase2End;
        } else if (phase === 3) {
          startVal = d.phase2End;
          endVal = d.phase3End;
        } else {
          startVal = d.phase3End;
          endVal = d.phase4End;
        }

        const value = startVal + (endVal - startVal) * clamped;
        this.redrawCashBar(bar, value);
      });
    }

    startStage(stageName) {
      this.activeStage = stageName;
      this.resetCircleHighlights();
      this.updateCircleProgress(0);
    }

    // --- Consumer Token System ---

    createConsumerTokens(payload) {
      if (!this.circle || !this.layers.tokens) {
        return;
      }
      this.layers.tokens.removeChildren();
      this.tokens = [];

      const people = payload.people || [];
      if (!people.length) {
        return;
      }

      const circle = this.circle;
      const useAggregate = people.length > 150;

      if (useAggregate) {
        this.createAggregateTokens(payload);
        return;
      }

      people.forEach((person) => {
        if (!person) {
          return;
        }
        const loc = Number(person.location);
        if (!Number.isFinite(loc)) {
          return;
        }
        const angle = -Math.PI / 2 + loc * Math.PI * 2;
        const ringX = circle.centerX + Math.cos(angle) * circle.radius;
        const ringY = circle.centerY + Math.sin(angle) * circle.radius;
        const jittered = jitterPoint({ x: ringX, y: ringY }, 10);

        const sprite = new PIXI.Graphics();
        sprite.beginFill(0x94a3b8, 0.88);
        sprite.drawCircle(0, 0, 3);
        sprite.endFill();
        sprite.x = jittered.x;
        sprite.y = jittered.y;
        sprite.alpha = 0;

        this.layers.tokens.addChild(sprite);

        // Find the hospital node for routing animation
        let hospitalNode = null;
        const targetHospitalId = person.hospitalId || person.nearestHospitalId;
        if (targetHospitalId !== undefined && targetHospitalId !== null && circle.hospitalNodes) {
          hospitalNode = circle.hospitalNodes.find((n) => n.id === targetHospitalId) || null;
        }

        this.tokens.push({
          sprite,
          person,
          homeX: jittered.x,
          homeY: jittered.y,
          ringAngle: angle,
          hospitalNode,
          originalColor: 0x94a3b8,
          targetColor: hexToInt(person.insurerColor || '#94a3b8'),
          glowSprite: null
        });
      });
    }

    createAggregateTokens(payload) {
      if (!this.circle || !this.layers.tokens) {
        return;
      }
      const circle = this.circle;
      const people = payload.people || [];

      // Group by insurer category + hospital
      const groups = new Map();
      people.forEach((person) => {
        if (!person) {
          return;
        }
        const category = person.insurerCategory || 'other';
        const hospitalId = person.hospitalId || person.nearestHospitalId || 0;
        const key = `${category}-${hospitalId}`;
        if (!groups.has(key)) {
          groups.set(key, {
            count: 0,
            color: person.insurerColor || '#94a3b8',
            category,
            hospitalId,
            location: Number(person.location) || 0,
            sick: false,
            bounce: false
          });
        }
        const group = groups.get(key);
        group.count += 1;
        if (person.sick) {
          group.sick = true;
        }
        if (person.bounce) {
          group.bounce = true;
        }
      });

      groups.forEach((group) => {
        const angle = -Math.PI / 2 + group.location * Math.PI * 2;
        const ringX = circle.centerX + Math.cos(angle) * circle.radius;
        const ringY = circle.centerY + Math.sin(angle) * circle.radius;
        const jittered = jitterPoint({ x: ringX, y: ringY }, 14);

        const radius = Math.max(4, Math.min(12, 3 + Math.sqrt(group.count)));
        const sprite = new PIXI.Graphics();
        sprite.beginFill(0x94a3b8, 0.88);
        sprite.drawCircle(0, 0, radius);
        sprite.endFill();
        sprite.x = jittered.x;
        sprite.y = jittered.y;
        sprite.alpha = 0;

        // Count badge
        const badgeStyle = new PIXI.TextStyle({
          fontFamily: 'Segoe UI, sans-serif',
          fontSize: 10,
          fill: 0xffffff,
          fontWeight: '700'
        });
        const badge = new PIXI.Text(String(group.count), badgeStyle);
        badge.anchor.set(0.5, 0.5);
        badge.x = 0;
        badge.y = 0;
        sprite.addChild(badge);

        this.layers.tokens.addChild(sprite);

        let hospitalNode = null;
        if (group.hospitalId && circle.hospitalNodes) {
          hospitalNode = circle.hospitalNodes.find((n) => n.id === group.hospitalId) || null;
        }

        this.tokens.push({
          sprite,
          person: {
            insurerColor: group.color,
            insurerCategory: group.category,
            sick: group.sick,
            bounce: group.bounce,
            stateEnrollmentTime: 15 + Math.random() * 5,
            movementStart: 30 + Math.random() * 5,
            travelTime: 10 + Math.random() * 20,
            arcHeight: 15 + Math.random() * 30,
            hospitalId: group.hospitalId
          },
          homeX: jittered.x,
          homeY: jittered.y,
          ringAngle: angle,
          hospitalNode,
          originalColor: 0x94a3b8,
          targetColor: hexToInt(group.color),
          glowSprite: null
        });
      });
    }

    updateInsurerBubbles(progress, phase) {
      if (!this.insurerBubbles || !this.insurerBubbles.length) {
        return;
      }
      const clamped = clamp(progress, 0, 1);

      this.insurerBubbles.forEach((bubble) => {
        let radius;
        let count;

        if (phase === 'shrink') {
          // Illness phase: shrink from full to (full - sick)
          const fromRadius = bubble.finalRadius;
          const toRadius = bubble.shrunkRadius;
          radius = fromRadius + (toRadius - fromRadius) * clamped;
          const fromCount = bubble.item.enrollees;
          const toCount = bubble.item.enrollees - bubble.item.sickCount;
          count = Math.round(fromCount + (toCount - fromCount) * clamped);
        } else {
          // Insurance phase: grow from 0 to full
          radius = Math.max(2, bubble.finalRadius * clamped);
          count = Math.round(bubble.item.enrollees * clamped);
        }

        bubble.gfx.clear();
        bubble.gfx.beginFill(bubble.color, 0.75);
        bubble.gfx.drawCircle(0, 0, radius);
        bubble.gfx.endFill();
        bubble.gfx.lineStyle(1.5, bubble.color, 0.4);
        bubble.gfx.drawCircle(0, 0, radius + 3);

        bubble.countLabel.text = String(Math.max(0, count));
        bubble.countLabel.alpha = clamped > 0.2 || phase === 'shrink' ? Math.min(1, phase === 'shrink' ? 1 : (clamped - 0.2) * 2.5) : 0;
        bubble.countLabel.visible = radius > 14;

        bubble.nameLabel.alpha = clamped > 0.15 || phase === 'shrink' ? Math.min(0.9, phase === 'shrink' ? 0.9 : (clamped - 0.15) * 2) : 0;
        bubble.nameLabel.y = bubble.y + radius + 4;
      });
    }

    updateHospitalBubbles(progress) {
      if (!this.hospitalBubbles || !this.hospitalBubbles.length) {
        return;
      }
      const clamped = clamp(progress, 0, 1);

      this.hospitalBubbles.forEach((bubble) => {
        // Delay growth until bullets have had time to arrive (~10% into routing)
        const delayed = clamp((clamped - 0.1) / 0.9, 0, 1);
        const radius = Math.max(2, bubble.finalRadius * delayed);
        const count = Math.round(bubble.patients * delayed);

        bubble.gfx.clear();
        bubble.gfx.beginFill(bubble.color, 0.7);
        bubble.gfx.drawCircle(0, 0, radius);
        bubble.gfx.endFill();
        bubble.gfx.lineStyle(1.5, bubble.color, 0.3);
        bubble.gfx.drawCircle(0, 0, radius + 3);

        bubble.countLabel.text = String(count);
        bubble.countLabel.alpha = clamped > 0.1 ? Math.min(1, (clamped - 0.1) * 2) : 0;
        bubble.countLabel.visible = radius > 12;
      });
    }

    updateTokensForInsurance(progress) {
      // Tokens stay hidden during insurance phase — insurer bubbles tell the story
      if (!this.tokens || !this.tokens.length) {
        return;
      }
      this.tokens.forEach((token) => {
        token.sprite.alpha = 0;
        // Pre-color tokens so they're ready for routing
        token.sprite.clear();
        token.sprite.beginFill(token.targetColor, 0.88);
        token.sprite.drawCircle(0, 0, 3);
        token.sprite.endFill();
      });
    }

    findInsurerBubblePos(person) {
      if (!this.insurerBubbles || !this.insurerBubbles.length) {
        return null;
      }
      const category = person.insurerCategory;
      const insurerId = person.insurerId;

      for (let i = 0; i < this.insurerBubbles.length; i++) {
        const b = this.insurerBubbles[i];
        if (category === 'uninsured' && b.item.category === 'uninsured') {
          return { x: b.x, y: b.y };
        }
        if (category === 'medicare' && b.item.category === 'medicare') {
          return { x: b.x, y: b.y };
        }
        if (category === 'private' && b.item.id === insurerId) {
          return { x: b.x, y: b.y };
        }
      }
      return null;
    }

    updateTokensForIllness(progress, payload) {
      if (!this.tokens || !this.tokens.length) {
        return;
      }
      const clamped = clamp(progress, 0, 1);

      this.tokens.forEach((token) => {
        const person = token.person;
        if (person.sick) {
          // Fade sick tokens in
          token.sprite.alpha = clamp(clamped * 3, 0, 0.92);

          // Move from ring position toward insurer bubble
          const bubblePos = this.findInsurerBubblePos(person);
          if (bubblePos) {
            const moveProgress = clamp((clamped - 0.15) / 0.7, 0, 1);
            const t = moveProgress * moveProgress * (3 - 2 * moveProgress); // smoothstep
            token.sprite.x = token.homeX + (bubblePos.x - token.homeX) * t;
            token.sprite.y = token.homeY + (bubblePos.y - token.homeY) * t;
            // Store the insurer position for routing phase
            token.insurerX = bubblePos.x;
            token.insurerY = bubblePos.y;
          }

          // Pulse effect in first 30% of illness phase
          if (clamped < 0.3) {
            const pulseProgress = clamped / 0.3;
            const scale = 1 + 0.6 * Math.sin(pulseProgress * Math.PI);
            token.sprite.scale.set(scale, scale);
          } else {
            token.sprite.scale.set(1, 1);
          }

          // Transition to red
          const colorProgress = clamp((clamped - 0.2) / 0.6, 0, 1);
          const sickColor = 0xef4444;
          const r1 = (token.targetColor >> 16) & 0xFF;
          const g1 = (token.targetColor >> 8) & 0xFF;
          const b1 = token.targetColor & 0xFF;
          const r2 = (sickColor >> 16) & 0xFF;
          const g2 = (sickColor >> 8) & 0xFF;
          const b2 = sickColor & 0xFF;
          const r = Math.round(r1 + (r2 - r1) * colorProgress);
          const g = Math.round(g1 + (g2 - g1) * colorProgress);
          const b = Math.round(b1 + (b2 - b1) * colorProgress);
          const mixed = (r << 16) | (g << 8) | b;

          token.sprite.clear();
          token.sprite.beginFill(mixed, 0.92);
          token.sprite.drawCircle(0, 0, 3);
          token.sprite.endFill();

          // Add glow behind sick tokens
          if (colorProgress > 0.5 && !token.glowSprite) {
            const glow = new PIXI.Graphics();
            glow.beginFill(sickColor, 0.25);
            glow.drawCircle(0, 0, 8);
            glow.endFill();
            glow.x = token.sprite.x;
            glow.y = token.sprite.y;
            this.layers.tokens.addChildAt(glow, 0);
            token.glowSprite = glow;
          }

          token.sprite.alpha = 0.95;
        } else {
          // Healthy tokens fade back
          token.sprite.alpha = clamp(0.85 - clamped * 0.55, 0.2, 0.85);
        }
      });
    }

    updateTokensForRouting(progress) {
      if (!this.tokens || !this.tokens.length || !this.circle) {
        return;
      }
      const clamped = clamp(progress, 0, 1);
      const circle = this.circle;

      this.tokens.forEach((token) => {
        const person = token.person;
        if (!person.sick) {
          // Non-sick tokens stay hidden
          token.sprite.alpha = 0;
          return;
        }

        // Bounced tokens flash red and fade at the uninsured bubble
        if (person.bounce) {
          if (clamped < 0.3) {
            const flash = Math.sin(clamped / 0.3 * Math.PI * 4);
            token.sprite.alpha = 0.5 + flash * 0.4;
          } else {
            token.sprite.alpha = clamp(0.9 - (clamped - 0.3) / 0.7 * 0.85, 0.05, 0.9);
          }
          return;
        }

        // Sick tokens travel from insurer bubble to hospital
        if (!token.hospitalNode) {
          return;
        }

        // Stagger movement start
        const moveStartFraction = clamp(Math.random() * 0.2, 0, 0.2);
        const moveDurationFraction = 0.6;
        const moveProgress = clamp((clamped - moveStartFraction) / moveDurationFraction, 0, 1);

        if (moveProgress <= 0) {
          return;
        }

        // Start from insurer bubble position (set during illness phase), or ring if not set
        const p0x = token.insurerX || token.homeX;
        const p0y = token.insurerY || token.homeY;
        const p2x = token.hospitalNode.sprite.x;
        const p2y = token.hospitalNode.sprite.y;

        // Smooth interpolation from insurer bubble to hospital node
        const t = moveProgress * moveProgress * (3 - 2 * moveProgress); // smoothstep
        const x = p0x + (p2x - p0x) * t;
        const y = p0y + (p2y - p0y) * t;

        token.sprite.x = x;
        token.sprite.y = y;
        token.sprite.alpha = clamp(0.95 - moveProgress * 0.3, 0.5, 0.95);

        // Move glow with token
        if (token.glowSprite) {
          token.glowSprite.x = x;
          token.glowSprite.y = y;
        }

        // Arrival effect: pulse hospital node
        if (moveProgress >= 0.95 && token.hospitalNode.sprite.alpha < 1) {
          token.hospitalNode.sprite.alpha = 1;
        }
      });
    }

    fadeTokensOut(duration) {
      if (!this.tokens || !this.tokens.length) {
        return;
      }
      this.tokens.forEach((token) => {
        gsap.to(token.sprite, { alpha: 0.1, duration: duration || 1 });
        if (token.glowSprite) {
          gsap.to(token.glowSprite, { alpha: 0, duration: duration || 1 });
        }
      });
    }

    // --- Illness Reveal ---

    playIllnessReveal(payload) {
      if (!this.app || !this.layout) {
        return;
      }
      const { width, height } = this.layout;

      // White flash overlay
      const flash = new PIXI.Graphics();
      flash.beginFill(0xffffff, 1);
      flash.drawRect(0, 0, width, height);
      flash.endFill();
      flash.alpha = 0;
      this.layers.overlays.addChild(flash);

      gsap.timeline()
        .to(flash, { alpha: 0.2, duration: 0.15 })
        .to(flash, { alpha: 0, duration: 0.65, onComplete: () => {
          if (flash.parent) {
            flash.parent.removeChild(flash);
          }
        }});

      // Sound
      this.playTone(200, 0.5);

      // Counter text
      const sickCount = payload.summary ? payload.summary.totalSick : 0;
      if (sickCount > 0) {
        const style = new PIXI.TextStyle({
          fontFamily: 'Segoe UI, sans-serif',
          fontSize: 28,
          fill: 0xf87171,
          fontWeight: '800',
          dropShadow: true,
          dropShadowColor: 0x0b132b,
          dropShadowBlur: 6,
          dropShadowDistance: 2
        });
        const text = new PIXI.Text(`${sickCount} consumers fell ill!`, style);
        text.anchor.set(0.5, 0.5);
        text.x = width / 2;
        text.y = this.layout.circle.centerY;
        text.alpha = 0;
        this.layers.overlays.addChild(text);

        gsap.timeline()
          .to(text, { alpha: 1, duration: 0.4, delay: 0.3 })
          .to(text, { alpha: 0, duration: 0.6, delay: 2.5, onComplete: () => {
            if (text.parent) {
              text.parent.removeChild(text);
            }
          }});
      }
    }

    // --- Financial Flow Particles ---

    initParticlePool() {
      this.particlePool = [];
      this.activeParticles = [];
      const poolSize = 40;
      for (let i = 0; i < poolSize; i++) {
        const sprite = new PIXI.Graphics();
        sprite.beginFill(0x22c55e, 0.9);
        sprite.drawCircle(0, 0, 3);
        sprite.endFill();
        sprite.visible = false;
        this.layers.tokens.addChild(sprite);
        this.particlePool.push({
          sprite,
          active: false,
          startX: 0, startY: 0,
          endX: 0, endY: 0,
          startTime: 0, duration: 1,
          color: 0x22c55e
        });
      }
    }

    spawnParticle(startX, startY, endX, endY, duration, color) {
      const particle = this.particlePool.find((p) => !p.active);
      if (!particle) {
        return;
      }
      particle.active = true;
      particle.startX = startX;
      particle.startY = startY;
      particle.endX = endX;
      particle.endY = endY;
      particle.startTime = performance.now();
      particle.duration = duration * 1000; // convert to ms
      particle.color = color || 0x22c55e;
      particle.sprite.clear();
      particle.sprite.beginFill(particle.color, 0.85);
      particle.sprite.drawCircle(0, 0, 3);
      particle.sprite.endFill();
      particle.sprite.x = startX;
      particle.sprite.y = startY;
      particle.sprite.visible = true;
      particle.sprite.alpha = 0.9;
      this.activeParticles.push(particle);
    }

    updateParticles() {
      const now = performance.now();
      for (let i = this.activeParticles.length - 1; i >= 0; i--) {
        const p = this.activeParticles[i];
        const elapsed = now - p.startTime;
        const t = clamp(elapsed / p.duration, 0, 1);

        if (t >= 1) {
          p.active = false;
          p.sprite.visible = false;
          this.activeParticles.splice(i, 1);
          continue;
        }

        const eased = t * t * (3 - 2 * t); // smoothstep
        p.sprite.x = p.startX + (p.endX - p.startX) * eased;
        p.sprite.y = p.startY + (p.endY - p.startY) * eased + Math.sin(t * Math.PI) * randomBetween(-8, 8);
        p.sprite.alpha = t < 0.8 ? 0.85 : 0.85 * (1 - (t - 0.8) / 0.2);
      }
    }

    spawnPremiumParticles(progress) {
      if (!this.tokens || !this.tokens.length || !this.layout) {
        return;
      }
      // Spawn a few particles per update, not every frame
      if (Math.random() > 0.15) {
        return;
      }
      const clamped = clamp(progress, 0.1, 0.9);
      // Pick a random enrolled token
      const enrolled = this.tokens.filter((t) => t.person.insurerCategory === 'private' && t.sprite.alpha > 0.3);
      if (!enrolled.length) {
        return;
      }
      const token = enrolled[Math.floor(Math.random() * enrolled.length)];
      const stageArea = this.layout.stageArea;
      this.spawnParticle(
        token.sprite.x, token.sprite.y,
        stageArea.x + stageArea.width * 0.3 + Math.random() * stageArea.width * 0.4,
        stageArea.y + 30 + Math.random() * 40,
        1.5 + Math.random(),
        0x22c55e
      );
    }

    spawnClaimParticles(progress) {
      if (!this.layout) {
        return;
      }
      if (Math.random() > 0.12) {
        return;
      }
      const stageArea = this.layout.stageArea;
      const midX = stageArea.x + stageArea.width / 2;
      this.spawnParticle(
        stageArea.x + Math.random() * stageArea.width * 0.4,
        stageArea.y + stageArea.height * 0.3 + Math.random() * stageArea.height * 0.4,
        midX + Math.random() * stageArea.width * 0.4,
        stageArea.y + stageArea.height * 0.3 + Math.random() * stageArea.height * 0.4,
        1.2 + Math.random() * 0.8,
        0x38bdf8
      );
    }

    // --- Routing Bullet Streams (insurer -> hospital) ---

    buildRoutingFlows(payload) {
      this.routingFlows = [];
      const people = payload.people || [];
      if (!people.length || !this.insurerBubbles || !this.hospitalBubbles) {
        return;
      }

      const flowMap = new Map();
      people.forEach((p) => {
        if (!p || !p.sick || !p.hospitalId) {
          return;
        }
        const insurerKey = p.insurerCategory === 'private' ? String(p.insurerId)
          : p.insurerCategory === 'medicare' ? 'medicare'
          : p.insurerCategory === 'uninsured' ? 'uninsured' : null;
        if (!insurerKey) {
          return;
        }
        const key = `${insurerKey}-${p.hospitalId}`;
        flowMap.set(key, (flowMap.get(key) || 0) + 1);
      });

      flowMap.forEach((count, key) => {
        const parts = key.split('-');
        const insurerKey = parts[0];
        const hospitalId = Number(parts[1]);

        const srcBubble = this.insurerBubbles.find((b) => String(b.item.id) === insurerKey);
        const dstBubble = this.hospitalBubbles.find((b) => b.hospitalId === hospitalId);
        if (!srcBubble || !dstBubble) {
          return;
        }

        this.routingFlows.push({
          srcX: srcBubble.x, srcY: srcBubble.y,
          dstX: dstBubble.x, dstY: dstBubble.y,
          count, color: srcBubble.color
        });
      });
    }

    spawnRoutingBullets(progress) {
      if (!this.routingFlows || !this.routingFlows.length) {
        return;
      }
      if (Math.random() > 0.25) {
        return;
      }

      const totalCount = this.routingFlows.reduce((sum, f) => sum + f.count, 0);
      let pick = Math.random() * totalCount;
      let flow = this.routingFlows[0];
      for (let i = 0; i < this.routingFlows.length; i++) {
        pick -= this.routingFlows[i].count;
        if (pick <= 0) {
          flow = this.routingFlows[i];
          break;
        }
      }

      const jitter = 6;
      this.spawnParticle(
        flow.srcX + randomBetween(-jitter, jitter),
        flow.srcY + randomBetween(-jitter, jitter),
        flow.dstX + randomBetween(-jitter, jitter),
        flow.dstY + randomBetween(-jitter, jitter),
        0.8 + Math.random() * 0.6,
        flow.color
      );
    }

    // --- Learning Annotations ---

    showAnnotation(message) {
      if (!this.annotationEl) {
        return;
      }
      this.annotationEl.textContent = message;
      this.annotationEl.classList.add('visible');
      if (this.annotationTimeout) {
        clearTimeout(this.annotationTimeout);
      }
      this.annotationTimeout = setTimeout(() => {
        if (this.annotationEl) {
          this.annotationEl.classList.remove('visible');
        }
      }, 4000);
    }

    // --- Round Summary ---

    showRoundSummary(payload) {
      if (!this.app || !this.layout) {
        return;
      }
      const { width, height } = this.layout;

      // Dark overlay
      const overlay = new PIXI.Graphics();
      overlay.beginFill(0x0b132b, 0.88);
      overlay.drawRect(0, 0, width, height);
      overlay.endFill();
      overlay.alpha = 0;
      this.layers.overlays.addChild(overlay);

      const titleStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: 26,
        fill: 0xf8fafc,
        fontWeight: '800'
      });
      const metricStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: 20,
        fill: 0xe2e8f0,
        fontWeight: '600'
      });
      const valueStyle = new PIXI.TextStyle({
        fontFamily: 'Segoe UI, sans-serif',
        fontSize: 22,
        fill: 0x38bdf8,
        fontWeight: '700'
      });

      const container = new PIXI.Container();
      container.alpha = 0;

      const title = new PIXI.Text('Round Complete', titleStyle);
      title.anchor.set(0.5, 0);
      title.x = width / 2;
      title.y = height * 0.15;
      container.addChild(title);

      const summary = payload.summary || {};
      const financials = payload.financials || {};
      const totalPeople = summary.totalPeople || 0;
      const totalSick = summary.totalSick || 0;
      const treated = summary.treatedPatients || 0;
      const bounced = summary.totalBounce || 0;
      const coverageRate = totalPeople > 0
        ? ((totalPeople - (payload.people || []).filter((p) => p && p.insurerCategory === 'uninsured').length) / totalPeople * 100).toFixed(0)
        : 0;

      const metrics = [
        { label: 'Coverage Rate', value: `${coverageRate}%` },
        { label: 'Fell Ill', value: `${totalSick} of ${totalPeople}` },
        { label: 'Treated', value: `${treated}`, color: 0x22c55e },
        { label: 'Denied Care', value: `${bounced}`, color: bounced > 0 ? 0xef4444 : 0x22c55e },
        { label: 'Hospital Profit', value: formatCurrency(financials.hospitalProfit || 0) },
        { label: 'Insurer Profit', value: formatCurrency(financials.insurerProfit || 0) }
      ];

      const startY = height * 0.3;
      const lineHeight = 36;
      metrics.forEach((metric, i) => {
        const labelText = new PIXI.Text(metric.label, metricStyle);
        labelText.anchor.set(1, 0.5);
        labelText.x = width / 2 - 20;
        labelText.y = startY + i * lineHeight;
        container.addChild(labelText);

        const valStyle = metric.color
          ? new PIXI.TextStyle({ ...valueStyle, fill: metric.color })
          : valueStyle;
        const valText = new PIXI.Text(metric.value, valStyle);
        valText.anchor.set(0, 0.5);
        valText.x = width / 2 + 20;
        valText.y = startY + i * lineHeight;
        container.addChild(valText);
      });

      this.layers.overlays.addChild(container);

      gsap.timeline()
        .to(overlay, { alpha: 1, duration: 0.8 })
        .to(container, { alpha: 1, duration: 0.6 }, '-=0.4');
    }

    resetCircleHighlights() {
      if (!this.circle || !this.circle.hospitalNodes) {
        return;
      }
      this.circle.hospitalNodes.forEach((node) => {
        node.sprite.alpha = 0.32;
      });
    }

    updateCircleProgress(progress) {
      if (!this.circle) {
        return;
      }
      const clamped = clamp(progress, 0, 1);

      // Move pointer around ring during insurance phase
      if (this.circle.pointer && this.activeStage === 'insurance') {
        const angle = -Math.PI / 2 + clamped * Math.PI * 2;
        this.circle.pointer.x = this.circle.centerX + Math.cos(angle) * this.circle.radius;
        this.circle.pointer.y = this.circle.centerY + Math.sin(angle) * this.circle.radius;
        this.circle.pointer.alpha = 0.95;
      } else if (this.circle.pointer) {
        this.circle.pointer.alpha = 0;
      }

      // Highlight hospital nodes
      if (this.circle.hospitalNodes && this.circle.hospitalNodes.length) {
        this.circle.hospitalNodes.forEach((node) => {
          node.sprite.alpha = clamped > 0.1 ? 0.85 : 0.32;
        });
      }
      this.circle.pointerProgress = clamped;
    }


    buildTimeline(payload) {
      if (this.timeline) {
        this.timeline.kill();
      }
      this.timeline = gsap.timeline({
        paused: true,
        onUpdate: () => {
          if (this.timeline) {
            this.updateTimelineUI(this.timeline.time());
            this.updateParticles();
          }
        }
      });
      const timeline = this.timeline;
      const phases = {};
      const phaseList = payload.timeline && Array.isArray(payload.timeline.phases)
        ? payload.timeline.phases
        : [];
      phaseList.forEach((phase) => {
        if (phase && phase.id) {
          phases[phase.id] = phase;
        }
      });

      const insurancePhase = phases.insurance || { start: 0, duration: 20 };
      const illnessPhase = phases.illness || { start: 20, duration: 10 };
      const routingPhase = phases.routing || { start: 30, duration: 40 };
      const profitPhase = phases.profit || { start: 70, duration: 50 };

      this.eventBook = (payload.timeline && payload.timeline.events ? payload.timeline.events : [])
        .slice()
        .sort((a, b) => a.time - b.time);

      const fallbackDuration = Math.max(profitPhase.start + profitPhase.duration + 6, 90);
      const totalSeconds = payload.timeline && Number(payload.timeline.totalSeconds)
        ? Number(payload.timeline.totalSeconds)
        : fallbackDuration;
      this.timelineDuration = totalSeconds;

      // --- Phase 1: Insurance Choice (premiums flow in) ---
      timeline.call(() => {
        this.startStage('insurance');
        this.playTone(440, 0.3);
      }, undefined, insurancePhase.start);

      const insuranceState = { progress: 0 };
      timeline.to(insuranceState, {
        progress: 1,
        duration: Math.max(insurancePhase.duration - 1, 6),
        ease: 'sine.inOut',
        onUpdate: () => {
          const progress = clamp(insuranceState.progress, 0, 1);
          this.updateCircleProgress(progress);
          this.updateInsurerBubbles(progress, 'grow');
          this.updateCashBars(1, progress);
        },
        onComplete: () => {
          this.updateCircleProgress(1);
          this.updateInsurerBubbles(1, 'grow');
          this.updateCashBars(1, 1);
        }
      }, insurancePhase.start + 0.4);

      // --- Phase 2: Illness Reveal + Care Costs ---
      timeline.call(() => {
        this.playIllnessReveal(payload);
      }, undefined, illnessPhase.start);

      const illnessState = { progress: 0 };
      timeline.to(illnessState, {
        progress: 1,
        duration: Math.max(illnessPhase.duration - 1, 4),
        ease: 'sine.inOut',
        onUpdate: () => {
          const progress = clamp(illnessState.progress, 0, 1);
          this.updateCashBars(2, progress);
        },
        onComplete: () => {
          this.updateCashBars(2, 1);
        }
      }, illnessPhase.start + 0.5);

      // --- Phase 3: Hospital Routing + Claims Paid ---
      timeline.call(() => {
        this.startStage('hospital');
        this.playTone(330, 0.15);
        setTimeout(() => this.playTone(440, 0.15), 180);
        this.pulseCircleRing();
      }, undefined, routingPhase.start);

      const hospitalState = { progress: 0 };
      timeline.to(hospitalState, {
        progress: 1,
        duration: Math.max(routingPhase.duration - 1, 6),
        ease: 'sine.inOut',
        onUpdate: () => {
          const progress = clamp(hospitalState.progress, 0, 1);
          this.updateCircleProgress(progress);
          this.updateInsurerBubbles(progress, 'shrink');
          this.updateHospitalBubbles(progress);
          this.spawnRoutingBullets(progress);
          this.updateCashBars(3, progress);
        },
        onComplete: () => {
          this.updateCircleProgress(1);
          this.updateInsurerBubbles(1, 'shrink');
          this.updateHospitalBubbles(1);
          this.updateCashBars(3, 1);
        }
      }, routingPhase.start + 0.4);

      // --- Phase 4: Public Payments (Medicare/DSH) ---
      timeline.call(() => {
        this.startStage('claims');
        this.pulseCircleRing();
      }, undefined, profitPhase.start);

      const profitState = { progress: 0 };
      timeline.to(profitState, {
        progress: 1,
        duration: Math.max(profitPhase.duration - 1, 6),
        ease: 'sine.inOut',
        onUpdate: () => {
          const progress = clamp(profitState.progress, 0, 1);
          this.updateCircleProgress(progress);
          this.updateCashBars(4, progress);
        },
        onComplete: () => {
          this.updateCircleProgress(1);
          this.updateCashBars(4, 1);
          this.playDepositSound();
        }
      }, profitPhase.start + 0.4);

      // --- Learning Annotations ---
      const annotations = payload.timeline && payload.timeline.annotations
        ? payload.timeline.annotations
        : [];
      annotations.forEach((ann) => {
        if (ann && ann.message && Number.isFinite(ann.time)) {
          timeline.call(() => this.showAnnotation(ann.message), undefined, ann.time);
        }
      });

      // --- Round Summary ---
      const summaryTime = Math.max(totalSeconds - 8, profitPhase.start + profitPhase.duration - 5);
      timeline.call(() => this.showRoundSummary(payload), undefined, summaryTime);
    }

    pulseCircleRing() {
      if (!this.circle || !this.circle.container) {
        return;
      }
      // Find the outer ring (first Graphics child)
      const ring = this.circle.container.children.find(
        (c) => c instanceof PIXI.Graphics && c !== this.circle.pointer && c !== this.circle.tail
      );
      if (!ring) {
        return;
      }
      const originalAlpha = ring.alpha;
      gsap.timeline()
        .to(ring, { alpha: 1, duration: 0.15 })
        .to(ring, { alpha: originalAlpha, duration: 0.4 });
    }


    updateTimelineUI(forcedTime) {
      if (!this.timeline) {
        return;
      }
      const currentTime = Number.isFinite(forcedTime) ? forcedTime : this.timeline.time();
      const total = this.timelineDuration || 1;
      const progress = clamp(total ? currentTime / total : 0, 0, 1);
      if (this.timelineProgressEl) {
        this.timelineProgressEl.style.width = `${progress * 100}%`;
      }
      if (this.timelineMarkerEl) {
        this.timelineMarkerEl.style.left = `${progress * 100}%`;
      }
      if (this.clockEl) {
        this.clockEl.textContent = formatClock(currentTime, total);
      }
      this.updateCaption(currentTime);
    }

    updateCaption(currentTime) {
      if (!this.captionEl) {
        return;
      }
      if (!this.eventBook.length) {
        this.captionEl.textContent = 'Timeline ready';
        return;
      }
      let active = this.eventBook[0];
      for (let i = 0; i < this.eventBook.length; i += 1) {
        if (currentTime >= this.eventBook[i].time) {
          active = this.eventBook[i];
        } else {
          break;
        }
      }
      if (this.lastCaption !== active.label) {
        this.captionEl.textContent = active.label;
        this.lastCaption = active.label;
      }
    }

    play() {
      if (!this.timeline) {
        return;
      }
      this.timeline.timeScale(this.state.speed || 1);
      this.timeline.play();
      this.state.playing = true;
      this.updatePlayButton();
    }

    pause(updateState = true) {
      if (!this.timeline) {
        return;
      }
      this.timeline.pause();
      if (updateState) {
        this.state.playing = false;
        this.updatePlayButton();
      }
    }

    updatePlayButton() {
      const button = this.controls.play;
      if (!button) {
        return;
      }
      const icon = button.querySelector('i');
      const label = button.querySelector('.label');
      if (icon) {
        icon.classList.remove('fa-play', 'fa-pause');
        icon.classList.add(this.state.playing ? 'fa-pause' : 'fa-play');
      }
      if (label) {
        label.textContent = this.state.playing ? 'Pause' : 'Play';
      }
    }

    setSpeed(speed) {
      const normalized = speed === 2 ? 2 : 1;
      this.state.speed = normalized;
      if (this.timeline) {
        this.timeline.timeScale(normalized);
      }

      if (this.controls.speed1) {
        this.controls.speed1.classList.toggle('active', normalized === 1);
      }
      if (this.controls.speed2) {
        this.controls.speed2.classList.toggle('active', normalized === 2);
      }
    }

    skipToNextPhase() {
      if (!this.timeline || !this.currentPayload) {
        return;
      }

      const currentTime = this.timeline.time();
      const phases = this.currentPayload.timeline && this.currentPayload.timeline.phases
        ? this.currentPayload.timeline.phases
        : [];

      // Find the next phase start time
      let nextPhaseTime = null;
      for (let i = 0; i < phases.length; i++) {
        const phaseStart = phases[i].start || 0;
        if (phaseStart > currentTime + 0.5) { // Add small buffer to avoid current phase
          nextPhaseTime = phaseStart;
          break;
        }
      }

      // If no next phase found, skip to the end
      if (nextPhaseTime === null) {
        nextPhaseTime = this.timelineDuration;
      }

      // Seek to the next phase
      this.timeline.seek(nextPhaseTime, false);
      this.updateTimelineUI(nextPhaseTime);

      // Resume playing if it was playing
      if (this.state.playing) {
        this.play();
      }
    }

    handleResize() {
      if (!this.currentPayload) {
        return;
      }
      // Skip resize if our root element is no longer in the document
      if (!this.root || !document.contains(this.root)) {
        return;
      }
      const resumePlaying = this.state.playing;
      const currentTime = this.timeline ? this.timeline.time() : 0;
      this.loadPayload(this.currentPayload, {
        autoPlay: false,
        initialTime: currentTime,
        resumePlaying
      });
      this.positionGroupHeadings();
      this.positionExternalPanelLabels();
    }
  }

  function handlePayload(payload, attempt) {
    if (!payload) {
      return;
    }
    const nsPrefix = payload.nsPrefix || '';
    const rootId = `${nsPrefix}market_sim_root`;
    const root = ensureElement(rootId);

    if (!root) {
      if ((attempt || 0) < MAX_ATTACH_ATTEMPTS) {
        setTimeout(() => handlePayload(payload, (attempt || 0) + 1), 150);
      }
      return;
    }

    let instance = INSTANCES.get(nsPrefix);
    if (instance && instance.root !== root) {
      // Modal was destroyed and recreated — discard the stale instance
      instance.resetScene();
      if (instance.audioCtx && instance.audioCtx.state !== 'closed') {
        instance.audioCtx.close();
      }
      if (instance.app) {
        instance.app.destroy(true, { children: true, texture: true, baseTexture: true });
      }
      INSTANCES.delete(nsPrefix);
      instance = null;
    }
    if (!instance) {
      instance = new MarketSimulationStoryboard(root, nsPrefix);
      INSTANCES.set(nsPrefix, instance);
    }

    instance.loadPayload(payload, { autoPlay: true });
  }

  if (Shiny && typeof Shiny.addCustomMessageHandler === 'function') {
    Shiny.addCustomMessageHandler(MESSAGE_TYPE, (payload) => {
      handlePayload(payload, 0);
    });
  }

  window.addEventListener('resize', () => {
    INSTANCES.forEach((instance, key) => {
      if (!instance.root || !document.contains(instance.root)) {
        // Evict stale instances whose modal is no longer in the DOM
        instance.resetScene();
        if (instance.audioCtx && instance.audioCtx.state !== 'closed') {
          instance.audioCtx.close();
        }
        if (instance.app) {
          instance.app.destroy(true, { children: true, texture: true, baseTexture: true });
        }
        INSTANCES.delete(key);
        return;
      }
      instance.handleResize();
    });
  });
})(window, document, window.Shiny, window.gsap, window.PIXI);
