import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { RouterModule } from '@angular/router';
import { HttpModule } from '@angular/http';
import { CommonModule } from '@angular/common';

import { OverviewComponent } from './overview.component';
import { AppRoutingModule } from './app-routing.module';
import { MonitoringService } from './monitoring.service';
import { BytePipe } from './byte.pipe';

@NgModule({
  imports: [
    BrowserModule,
    AppRoutingModule,
    HttpModule,
    CommonModule,
  ],

  declarations: [
    OverviewComponent,
    BytePipe,
  ],

  bootstrap: [OverviewComponent],

  providers: [MonitoringService],
})

export class AppModule { }
