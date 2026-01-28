// TO RUN THIS NODE SCRIPT, MAKE SURE YOU ARE IN THE LATEST VERSION OF NODE (nvm use 20)
// RUN THIS SCRIPT FROM THE ROOT DIRECTORY "BOEK"

const fs = require('fs');
const path = require('path');
const puppeteer = require('puppeteer');
const { PDFDocument, rgb } = require('pdf-lib');

// Define your HTML file paths or URLs here
const links = [
  '_book/index.html',
  '_book/cursusinformatie.html',
  '_book/links.html',
  '_book/Modellen/intro_modellen.html',
  '_book/Modellen/exponentielegroei.html',
  '_book/Modellen/odes_basis.html',
  '_book/Modellen/dichtheidseffecten.html',
  '_book/Modellen/ruimtelijkegroei.html',
  '_book/Modellen/predatorprooi.html',
  '_book/Modellen/odes_grind.html',
  '_book/Modellen/competitie.html',
  '_book/Modellen/zwermen.html',
  '_book/Modellen/reactiediffusie.html',
  '_book/Modellen/exciteerbaremedia.html',
  '_book/Modellen/project.html',

  '_book/Statistiek/Inleiding.html',
  '_book/Statistiek/BeschrijvendeStatistiek.html',
  '_book/Statistiek/Onzekerheid.html',
  '_book/Statistiek/Kansrekenen.html',
  '_book/Statistiek/Kansverdelingen.html',
  '_book/Statistiek/Schatten.html', 
  '_book/Statistiek/Hypothesetoetsen.html',
  '_book/Statistiek/ToetsenGemiddelde.html',
  '_book/Statistiek/AssociatiesTussenVariabelen.html',
  '_book/Statistiek/LineaireRelatiesEnFitten.html',
  '_book/Appendices/dankwoord.html'
  
  // '_book/Programmeren/introductie_R.html',
  // '_book/Programmeren/introductie_R2.html',
  // '_book/Programmeren/intro_R_questions.html',
  // '_book/Wiskunde/inleiding.html',
  // '_book/Wiskunde/algebra.html',
  // '_book/Wiskunde/breuken.html',
  // '_book/Wiskunde/exponenten.html',
  // '_book/Wiskunde/limieten.html',
  // '_book/Wiskunde/afgeleiden.html',
  // '_book/Wiskunde/schetsen.html',
  // '_book/Wiskunde/wiskunde_questions.html',

  

  //'_book/Modellen/oefenvragen.html',

  

  //'_book/Statistiek/Oefenvragen.html',
  //'_book/Appendices/antwoorden_R.html',
  //'_book/Appendices/antwoorden_wiskunde.html',
  //'_book/Appendices/antwoorden_modellen.html',
  //'_book/Appendices/antwoorden_statistiek.html',
  
];

(async () => {
  const browser = await puppeteer.launch();
  console.log("Puppeteer browser launched.");
  const tempFiles = [];

  // Step 1: Generate PDFs from each link
  for (let i = 0; i < links.length; i++) {
    const page = await browser.newPage();

    const url = links[i].startsWith('http') ? links[i] : `file://${path.resolve(links[i])}`;
    console.log('➡️ goto', links[i]);
    await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 60000 });
    

    const tempFile = `temp_chapter_${i + 1}.pdf`;
    await page.evaluateHandle('document.fonts.ready');
    // Inject custom styles for two-column layout
    await page.emulateMediaType('print'); 
    
    await new Promise(resolve => setTimeout(resolve, 200));

    // Remove the unwanted elements
    await page.evaluate(() => {
      document.querySelectorAll(`
        /* breadcrumbs */
        .quarto-title-breadcrumbs,
        nav[aria-label="breadcrumb"],
        .breadcrumb,

        /* search UI */
        #quarto-search,
        .quarto-search,
        #quarto-search-results,

        /* navbar / secondary nav, but NOT the title block */
        .navbar,
        #quarto-navbar,
        .quarto-secondary-nav,
        .headroom,

        /* sidebar toggle / book icon */
        .sidebar-toggle,
        .footer
      `).forEach(el => el.remove());
    });

    console.log("Page evaluated.");
    // Haal ook het "Code ▸" ding weg, dat doet nu niets. Maar het figuur moet blijven dus iets meer gepriegel. 
    await page.evaluate(() => {
      const root = document.querySelector("main") || document.body;

      // 1) Als code in <details> zit: haal wrapper weg (zodat er geen "details ding" overblijft)
      root.querySelectorAll("details").forEach(details => {
        // verwijder de "Code ▸" summary als die er is
        details.querySelectorAll("summary").forEach(s => s.remove());

        // verwijder code-input binnen details
        details.querySelectorAll(`
          div.sourceCode,
          pre.sourceCode,
          .cell-code,
          .cell-input,
          .quarto-code-cell,
          .code-with-copy
        `).forEach(el => el.remove());

        // unwrap: verplaats resterende kinderen (outputs!) naar buiten
        const parent = details.parentNode;
        while (details.firstChild) parent.insertBefore(details.firstChild, details);
        details.remove();
      });

      // 2) Verwijder ook code-input die NIET in details zit (maar laat outputs staan)
      root.querySelectorAll(`
        div.sourceCode,
        pre.sourceCode,
        .cell-code,
        .cell-input,
        .code-with-copy
      `).forEach(el => el.remove());

      // 3) Verwijder eventuele losse "Code" knoppen (zonder de code/output aan te raken)
      root.querySelectorAll(`
        .code-fold-btn,
        .code-fold-toggle,
        .quarto-code-fold,
        button.code-fold-btn
      `).forEach(el => el.remove());
    });

    

      await page.pdf({
        path: tempFile,
        format: 'a4',
        //width: '176mm', //b5
        //height: '250mm',//b5
        scale: 0.95,
        preferCSSPageSize: true,
        printBackground: true,
        margin: { top: '15mm', bottom: '25mm', right: '15mm', left: '15mm' }
      });

      tempFiles.push(tempFile);
      await page.close();
      console.log('✅ done with URL ' + (i + 1) + ': ' + links[i] + ' -> ' + tempFile);
  }

  await browser.close();

  // Add cover.pdf to the front of the tempFiles list
  tempFiles.unshift('figuren/blank.pdf');
  tempFiles.unshift('figuren/cover.pdf');
  tempFiles.push('figuren/blank.pdf');
  // Step 2: Merge PDFs and add running page numbers
  const mergedPdf = await PDFDocument.create();
  let pageIndex = 0;
  let totalPages = -3;
  for (const file of tempFiles) {
    const pdfBytes = fs.readFileSync(file);
    const pdfDoc = await PDFDocument.load(pdfBytes);
    const pages = await mergedPdf.copyPages(pdfDoc, pdfDoc.getPageIndices());
    totalPages += pages.length;
  }
  for (const file of tempFiles) {
    const pdfBytes = fs.readFileSync(file);
    const pdfDoc = await PDFDocument.load(pdfBytes);
    const pages = await mergedPdf.copyPages(pdfDoc, pdfDoc.getPageIndices());
    
    if (file === 'figuren/cover.pdf') {
        const page = pages[0];
        mergedPdf.addPage(page);
        const pageWidth = page.getWidth();
        const fontSize = 20;
        
        const currentYear = new Date().getFullYear();
        const yearText = `${currentYear-1}-${currentYear}`;
        const textWidth = fontSize * yearText.length * 0.5;
        const x = (pageWidth - textWidth) / 2;
        const font = await mergedPdf.embedFont('Helvetica-Bold');
        page.drawText(yearText, {
          x: x,
          y: 30,
          size: fontSize,
          color: rgb(0.3, 0.3, 0.3),
          font: font,
        });
    }
    else if (file === 'figuren/blank.pdf') {
        const page = pages[0];
        mergedPdf.addPage(page);
    }
    // Else it's a "chapter" page, add numbers
    else {
      for (const page of pages) {
        const gutterPt = 12; // extra inner margin in points (try 10–18)
        const w = page.getWidth();
        const h = page.getHeight();

        const [embedded] = await mergedPdf.embedPages([page]);

        // pageIndex is 0-based before increment; after ++pageIndex it’s 1-based.
        // If you want odd pages = right-hand pages: 1,3,5... -> shift right (more left margin).
        const nextNumber = pageIndex + 1;
        const isRightHand = (nextNumber % 2 === 1);

        // Shift content away from the spine:
        // right-hand page (odd): increase LEFT margin => move content RIGHT (+gutter)
        // left-hand page (even): increase RIGHT margin => move content LEFT (-gutter)
        const shiftX = isRightHand ? gutterPt : -gutterPt;

        const newPage = mergedPdf.addPage([w, h]);
        newPage.drawPage(embedded, { x: shiftX, y: 0 });

        // now draw your page number on newPage (not on `page`)
        const fontSize = 7;
        pageIndex++;
        newPage.drawText(`${pageIndex}/${totalPages}`, {
          x: (w / 2) - 10,
          y: 10,
          size: fontSize,
          color: rgb(0.5, 0.5, 0.5),
        });
      }
    }
  }

  const finalPdfBytes = await mergedPdf.save();
  fs.writeFileSync('final_book.pdf', finalPdfBytes);

  // Optional cleanup
  // tempFiles.forEach(f => {
  //   if (f.startsWith('temp_chapter_')) {
  //     fs.unlinkSync(f);
  //   }
  // });

  console.log('📘 Done: final_book.pdf with continuous page numbers.');
})();
