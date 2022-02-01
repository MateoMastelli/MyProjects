
--La información con la que voy a trabajar es un Dataset con datos del Covid-19 que descargue de la página https://ourworldindata.org/covid-deaths

Select *
From PortfolioProject..CovidDeaths
order by 3,4


--Selecciono la infomración que voy a estar usando
Select location, date, total_cases,new_cases,total_deaths,population
From PortfolioProject..CovidDeaths
order by 1,2

--Comparo los Casos Totales vs Muertes totales y lo filtro por el pais Argentina
--Muestra que tan probable es morir si te contagias Covid-19 en Argetina (Dato crudo sin considerar contexto económico ni social de las personas) 
Select location, date, total_cases,total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths
Where location like'%Argentina%'
order by 1,2


-- Comparo los Casos Totales vs la Población
-- Muestra que porcentaje de la población tuvo Covid en Argentina
Select location, date, population, total_cases, (total_cases/population)*100 as CovidPositivePercentange
From PortfolioProject..CovidDeaths
Where location like'%Argentina%'
order by 1,2


-- Busco los paises con mayor Tasa de Contagio vs la Población 
Select location,population, MAX(total_cases) as HighestInfectionCount, MAX((total_cases/population))*100 as CovidPositivePercentange
From PortfolioProject..CovidDeaths
Group by location, population  
order by CovidPositivePercentange desc



-- Busco los paises con la mayor cantidad de muertos teniendo en función de la Población
Select location,population, MAX(cast(total_deaths as int)) as TotalCovidDeathCount, MAX((total_deaths/population))*100 as CovidDeathPercentange
From PortfolioProject..CovidDeaths
where continent<>''
Group by location, population  
order by CovidDeathPercentange desc



--Continentes con la mayor Cantidad de Muertos en función de la Población
Select continent, MAX(cast(total_deaths as int)) as TotalCovidDeathCount
From PortfolioProject..CovidDeaths
where continent<>''
Group by continent  
order by TotalCovidDeathCount desc	


-- Números globables
Select date, sum(new_cases) as TotalCases, sum(cast(new_deaths as int)) as TotalDeaths, sum(cast(new_deaths as int))/sum(new_cases)*100 as DeathPercentange
From PortfolioProject..CovidDeaths
Where continent <> '' 
group by date
order by 1,2


-- Población Total vs Vacunados
-- Para realizar esta busqueda juntamos ambos tableros
with PopVsVac(Continent, Location, Date, Population,new_vaccinations, RollingPeopleVaccinated)
as
(
Select dea.continent, dea.location,dea.date, dea.population, vac.new_vaccinations
,sum(convert(float,vac.new_vaccinations)) OVER (partition by dea.location Order by dea.location, dea.date) as RollingPeopleVaccinated
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac on dea.location = vac.location and dea.date=vac.date	 
Where dea.continent <> '' 
--order by 2,3
)
Select*, (RollingPeopleVaccinated/Population)*100
From PopVsVac


-- Creación de talba
Drop table if exists #PerctPopulationVaccinated
Create Table #PerctPopulationVaccinated
(
Continent nvarchar(255),location nvarchar(255), Date datetime, Population numeric, New_Vaccinations nvarchar(255), RollingPeopleVaccinated float
)

Insert into #PerctPopulationVaccinated
Select dea.continent, dea.location,dea.date, dea.population, vac.new_vaccinations
,sum(convert(float,vac.new_vaccinations)) OVER (partition by dea.location Order by dea.location, dea.date) as RollingPeopleVaccinated
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac on dea.location = vac.location and dea.date=vac.date	 
Where dea.continent <> '' 
order by 2,3

Select*, (RollingPeopleVaccinated/Population)*100
From #PerctPopulationVaccinated


-- Creación de view para guardar la información para futuras visualizaciones
Create View PerctPopulationVaccinated as
Select dea.continent, dea.location,dea.date, dea.population, vac.new_vaccinations
,sum(convert(float,vac.new_vaccinations)) OVER (partition by dea.location Order by dea.location, dea.date) as RollingPeopleVaccinated
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac on dea.location = vac.location and dea.date=vac.date	 
Where dea.continent <> '' 
--order by 2,3

Select *
from PerctPopulationVaccinated



Select continent, MAX(cast(total_deaths as int)) as TotalCovidDeathCount
From PortfolioProject..CovidDeaths
where continent<>''
Group by continent  
order by TotalCovidDeathCount desc	