module Buki.StaticFrontend.Welcome.Views where

import Buki.StaticFrontend.Core.Preludes.API (Html)
import Buki.StaticFrontend.Core.ViewM
import qualified Text.Blaze.Html5 as H
import Data.Default (def)
import Buki.StaticFrontend.Core.Views.RegularPage
import Data.Text (Text)
import qualified Text.Blaze.Html5.Attributes as HA
import Buki.StaticFrontend.Welcome.API
import Data.Proxy

welcomePage :: [(Text, Text)] -> ViewM Html
welcomePage categories = do
  termsOfUsePageLink <- mkLink (Proxy @WelcomeAPI) (Proxy @TermsOfUseRoute)
  regularPage def { regularPageSubTitle = Just "Willkommen" } $ do
    H.p $ do
      "Über die Kindergartenbücherei Rohrendorf können Sie einen Großteil "
      "jener Bücher des Kindergartens Rohrendorf ausborgen, die derzeit im "
      "pädagogischen Alltag nicht laufend verwendet werden."
    H.p $ do
      "Bücher können über die Homepage oder am Dienstag, 07:30-08:00 persönlich "
      "ausgeliehen werden. Ausgeliehene Bücher können Sie ab Dienstag Mittag "
      "gemeinsam mit Ihrem Kind in der Gruppe Ihres Kindes abholen. Bitte geben "
      "Sie ausgeliehene Bücher bis zum Folgenden Dienstag, 08:00 in der "
      "Kindergartengruppe Ihres Kindes wieder zurück."
    H.p $ do
      "Da die Bücherausgabe, -rücknahme und Sortierung durch Freiwillige Helfer "
      "erfolgt, ist eine kurzfristigere oder längere Ausleihe derzeit organisatorisch "
      "nicht möglich."
    H.p $ do
      "Die Ausleihe von Büchern ist kostenlos, wir freuen uns jedoch über "
      "freiwillige Spenden, die dem Kindergarten zur freien Verfügung stehen. Alles "
      "weitere entnehmen Sie bitten den "
      H.a H.! HA.href (H.toValue termsOfUsePageLink) $ "Nutzungsbedingungen"
      "."

termsOfUsePage :: ViewM Html
termsOfUsePage = do
  regularPage def { regularPageSubTitle = Just "Nutzungsbedingungen" } $ do
    H.p $ do
      "Der Kindergarten Rohrendorf verleiht an in ihm betreute Kinder und deren Eltern, "
      "ausgewählte Bücher unentgeltlich und zum eigenen Gebrauch, sofern diese Bücher zum "
      "Zeitpunkt der Ausleihe nicht im Kindergarten benötigt werden."
    H.p $ do
      "Teile der Elternschaft (im Folgenden »Betreiber«) des Kindergarten Rohrendorfs "
      "organisieren die Ausleihe, um Eltern und Kindern (im Folgenden einfach »Eltern«) "
      "Zugang zu zahlreichen Kinderbüchern zu ermöglichen, ohne das das "
      "Kindergartenpersonal zusätzlich belastet wird. Die erbrachten Leistungen schließen "
      "die Ausgabe, Rücknahme, Inventarisierung und Sortierung von Büchern, sowie den "
      "Betrieb dieser Internetseite mit ein."
    H.p $ do
      "Die erbrachte Leistung wird unter größtem Bemühen innerhalb des dafür "
      "vorgesehenen zeitlichen Rahmens erbracht. Zu keinem Zeitpunkt ergibt sich für "
      "Eltern daraus jedoch ein Anspruch, bestimmte Bücher zu einem bestimmten Zeitpunkt "
      "oder für einen bestimmten Zeitraum zu erhalten. Insbesondere können Reservierungen, "
      "die über diese Internetseite getätigt werden, jederzeit und ohne Angabe von "
      "Gründen storniert werden."
    H.p $ do
      "Eltern können über diese Internetseite Bücher reservieren. Bücher werden in der "
      "Regel am ersten Tag der Ausleihe bis 12:00 an einem der während der Registrierung "
      "von Eltern angegebenen Orte abgelegt. Sollte ein Buch als »ausgegeben« markiert "
      "worden sein, jedoch nicht am angegebenen Ort liegen, so ist das den Betreibern "
      "unverzüglich zu melden. Sollte ein Buch andernfalls verloren gehen oder beschädigt "
      "werden, so erklären sich teilnehmende Eltern bereit, den entstandenen Schaden zu "
      "ersetzen."
    H.p $ do
      "Die Ausleihe ist kostenlos. Um den Kindergarten Rohrendorf bei den durch erhöhte "
      "Nutzung zusätzlich entstehenden Kosten zu unterstützen, steht es Eltern frei, für "
      "ausgeliehene Bücher zu spenden. Die Betreiber senden dazu während des "
      "Kindergartenjahres bis zu 4-mal an die während der Registierung angegebene "
      "E-Mail-Adresse eine E-Mail mit allen im angegebenen Zeitraum ausgeliehenen "
      "Büchern. Die E-Mail wird um einen Spendenvorschlag von 0,20€ je ausgeliehenem "
      "Buch ergänzt. Sämtliche Spendeneinnahmen stehen dem Kindergarten Rohrendorf zur "
      "freien Verfügung."
