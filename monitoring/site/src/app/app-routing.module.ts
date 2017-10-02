import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { OverviewComponent } from './overview.component';

const routes: Routes = [
  // TODO : fixer le double OverviewComponent
  // { path: '', redirectTo: '/overview', pathMatch: 'full' },
  // { path: 'overview', component: OverviewComponent },

  { path: '', component: OverviewComponent },

  // { path: 'cpu', component: OverviewComponent },
  // { path: 'mem', component: MemoryComponent },
  // { path: 'net', component: NetworkComponent },
  // { path: 'disk', component: DiskComponent }
  // { path: 'process', component: ProcessComponent },
  // { path: 'history', component: HistoryComponent },
  // { path: 'settings', component: SettingsComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})

export class AppRoutingModule { }
